# Setup Reference

Personal dev-environment config on this Mac. Captures the current state of
tmux sessions, SSH tunnels, and related config so it doesn't have to be
rediscovered. **Read this first** when working on any of these pieces.

Last verified: 2026-07-13

---

## Config files map

| File | Purpose |
|------|---------|
| `~/.config/tmuxinator/work.yml` | `work` tmux session (attached, conda glam) |
| `~/.config/tmuxinator/ide.yml` | `ide` tmux session (detached, for IDE to attach) |
| `~/.ssh/config` | Host blocks for `203.0.113.7`, `203.0.113.8`, `github.com` |
| `~/Library/LaunchAgents/local.ssh-tunnel-8003.plist` | launchd: SSH tunnel → `203.0.113.7:8003` |
| `~/Library/LaunchAgents/local.ssh-tunnel-8000.plist` | launchd: SSH tunnel → `203.0.113.8:8000` |
| `~/.zshrc` | `export EDITOR='vim'` (line ~85), oh-my-zsh, mamba init, pi PATH |

Versions: tmux 3.6a (nix-profile), tmuxinator 3.3.7 (nix-profile), macOS 14.x.

tmuxinator config dir resolution (in order): `$TMUXINATOR_CONFIG` →
`$XDG_CONFIG_HOME/tmuxinator` → `~/.config/tmuxinator`. On this machine both
env vars are unset, so it uses `~/.config/tmuxinator/`.

---

## tmux sessions

### `work` — `tmuxinator start work`

Attached, general-purpose. conda env `glam` activated in **every pane** via
`pre_window`:

```yaml
pre_window:
  - 'eval "$(/Users/grapefroot/miniforge3/bin/conda shell.zsh hook)"'
  - conda activate glam
```

Two windows:

| # | Window | Layout | Panes |
|---|--------|--------|-------|
| 1 | `main` | `even-horizontal` | `top`, `bottom` (left/right halves) |
| 2 | `split` | custom 50/25/25 | `left` (50%), `tr` (25%), `br` (25%) |

### `ide` — `tmuxinator start ide [project-root]`

**Detached** (`attach: false`) — an IDE attaches to it on its own terms.
Root is parameterized: `tmuxinator start ide ~/projects/foo` (defaults to
`$HOME`). `startup_window: shell` lands on the single-pane window on start.

Two windows:

| # | Window | Layout | Panes |
|---|--------|--------|-------|
| 1 | `shell` | default (1 pane) | `shell` ← active on start |
| 2 | `split` | custom 50/25/25 | `main` (50%), `build` (25%), `logs` (25%) |

### The 50/25/25 layout string

Both `split` windows use a captured custom layout (not `main-vertical`, which
defaults the main pane to >50%):

```yaml
layout: '5dfa,360x90,0,0{180x90,0,0,12,179x90,181,0[179x45,181,0,13,179x44,181,46,14]}'
```

This is tmux's compact layout format. It **scales proportionally** on window
resize (verified: at 120-wide → 60/59/59, still 50/25/25). If it ever behaves
oddly, recapture from a fresh window (known tmuxinator issue #651).

### tmux layout naming gotcha

tmux names layouts by *arrangement direction*, not divider direction — the
opposite of vim convention:
- "vertical split" (vim `:vsplit`, left/right) = tmux **`even-horizontal`**
- "horizontal split" (vim `:split`, top/bottom) = tmux **`even-vertical`**

### tmux sessions do NOT survive reboot

tmux is in-memory. After a reboot the server is gone and `tmux ls` errors with
`error connecting to /private/tmp/tmux-502/default (No such file or directory)`.
Restart manually:

```bash
tmuxinator start work
tmuxinator start ide
```

To rebuild a live session from the (edited) config:
```bash
tmux kill-session -t work && tmuxinator start work
```

---

## SSH tunnels (launchd)

Two persistent SSH local-forwards, managed as user LaunchAgents. Start at
login, auto-restart on failure (`KeepAlive` + `ThrottleInterval=10`).

| Service label | Local | Remote | Plist |
|---------------|-------|--------|-------|
| `local.ssh-tunnel-8003` | `localhost:8003` | `203.0.113.7:8003` | `~/Library/LaunchAgents/local.ssh-tunnel-8003.plist` |
| `local.ssh-tunnel-8000` | `localhost:8000` | `203.0.113.8:8000` | `~/Library/LaunchAgents/local.ssh-tunnel-8000.plist` |

Both run:
```
ssh -N -o ExitOnForwardFailure=yes -o ServerAliveInterval=60 -o ServerAliveCountMax=3 \
    -L <port>:localhost:<port> <host>
```

Host options (`StrictHostKeyChecking=no`, `ForwardAgent=yes`, `AddKeysToAgent=yes`,
`User=ubuntu`) come from `~/.ssh/config`, not the plist.

Why each ssh flag:
- **`-N`** — no remote shell; pure forwarding (essential for a background service).
- **`ExitOnForwardFailure=yes`** — if the local port can't bind (already in use), ssh exits instead of hanging; `KeepAlive` then retries cleanly.
- **`ServerAliveInterval=60` / `ServerAliveCountMax=3`** — detect dead connections through NAT/firewalls so launchd can restart them.

Logs: `~/Library/Logs/ssh-tunnel-<port>.{log,err.log}`.

### Management commands (replace `<svc>` with the label)

```bash
# status
launchctl print gui/$(id -u)/<svc> | grep -E 'state|pid|last exit'

# is the port listening?
lsof -nP -iTCP:<port> -sTCP:LISTEN

# bounce in place
launchctl kickstart -k gui/$(id -u)/<svc>

# unload (stops until next login or manual bootstrap)
launchctl bootout gui/$(id -u)/<svc>

# load / reload after editing the plist
launchctl bootout   gui/$(id -u)/<svc> 2>/dev/null
launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/<svc>.plist

# uninstall completely
launchctl bootout gui/$(id -u)/<svc> && rm ~/Library/LaunchAgents/<svc>.plist
```

### No-internet-at-startup behavior

If there's no network at login, ssh fails → launchd retries every ~10s
(`ThrottleInterval`). The moment internet returns, the next retry succeeds.
Port stays closed (no listener) until ssh connects — correct, since there's
nothing to reach. `last exit code` shows `255` during the gap.

Caveat: no `ConnectTimeout` is set, so when the remote is unreachable-but-
DNS-works, ssh hangs ~75s before failing. Optional improvement: add
`-o ConnectTimeout=10` to the plist's `ProgramArguments` for a faster retry
loop (~20s instead of ~85s).

---

## SSH config (`~/.ssh/config`)

```sshconfig
Host 203.0.113.7
  HostName 203.0.113.7
  StrictHostKeyChecking no
  ForwardAgent yes
  AddKeysToAgent yes
  User ubuntu

Host 203.0.113.8
  HostName 203.0.113.8
  StrictHostKeyChecking no
  ForwardAgent yes
  AddKeysToAgent yes
  User ubuntu

Host github.com
  AddKeysToAgent yes
  IdentityFile ~/.ssh/id_ed25519
```

- Key `~/.ssh/id_ed25519` has **no passphrase** → ssh reads it from disk at
  login; tunnels come up at reboot without any keychain/agent dependency.
- `AddKeysToAgent yes` loads the key into the agent on first connect, enabling
  agent forwarding for onward ssh/git from the remotes.

---

## Shell (`~/.zshrc`)

- oh-my-zsh, `ZSH_THEME="lambda"`, `plugins=(git)`
- `export EDITOR='vim'` (line ~85) — needed for `tmuxinator edit`, `git commit`, etc.
- mamba/micromamba init block (managed by `mamba shell init`)
- `export PATH="$HOME/.local/bin:$PATH"` (pi)
- conda is at `/Users/grapefroot/miniforge3/bin/conda` (used by `work.yml`'s `pre_window`)

`$EDITOR` was previously unset; setting it fixed `tmuxinator doctor`'s
"Checking if $EDITOR is set ==> No".

---

## Key relationships / why things are where they are

- **Tunnels are in launchd, not tmuxinator** — `on_project_start` runs in the
  launching shell and *blocks* session creation until it returns; `ssh -L` is a
  foreground tunnel that never returns, so the session would hang forever.
  launchd owns the tunnel lifecycle; any process on the Mac (including tmux
  panes) just talks to `localhost:<port>`.
- **conda activation is in `pre_window`, not `on_project_start`** —
  `on_project_start` runs once outside tmux; `pre_window` runs in *every pane*
  via `send-keys`, so `glam`'s scripts are on `$PATH` everywhere in the session.
- **`work` is attached, `ide` is detached** — the IDE attaches to `ide` on its
  own terms; `work` is for driving directly in the terminal.

---

## Open items / optional improvements

- `ide` session's pane names are still the original placeholders
  (`main`/`build`/`logs`); no auto-run commands set. Add commands to panes
  (e.g. `- build: cargo watch -x test`) when ready.
- Optional: add `-o ConnectTimeout=10` to both tunnel plists for faster
  retry loops when offline.
- Optional: shell aliases in `~/.zshrc` (`alias mux='tmuxinator'`,
  `alias mw='tmuxinator start work'`, `alias mi='tmuxinator start ide'`).
