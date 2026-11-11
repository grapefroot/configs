# Setup Reference

Personal dev-environment config on this Mac. Captures the current state of
tmux sessions, SSH config, and related config so it doesn't have to be
rediscovered. **Read this first** when working on any of these pieces.

Last verified: 2026-07-13 (SSH tunnels section updated 2026-11-12)

---

## Config files map

| File | Purpose |
|------|---------|
| `~/.config/tmuxinator/work.yml` | `work` tmux session (attached, conda glam) |
| `~/.config/tmuxinator/ide.yml` | `ide` tmux session (detached, for IDE to attach) |
| `~/.ssh/config` | Host blocks for `203.0.113.7`, `203.0.113.8`, `github.com` |
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

## SSH tunnels (removed)

The two launchd tunnels (`local.ssh-tunnel-8003` → `203.0.113.7:8003`,
`local.ssh-tunnel-8000` → `203.0.113.8:8000`) were retired on 2026-11-12
because access to the remote machines is gone. If either is still installed
on the Mac, uninstall it:

```bash
for svc in local.ssh-tunnel-8003 local.ssh-tunnel-8000; do
  launchctl bootout gui/$(id -u)/$svc 2>/dev/null
  rm -f ~/Library/LaunchAgents/$svc.plist
done
rm -f ~/Library/Logs/ssh-tunnel-800{0,3}.{log,err.log}
```

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

- Key `~/.ssh/id_ed25519` has **no passphrase** → ssh reads it from disk
  without any keychain/agent dependency.
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

- **Never start long-running processes (e.g. `ssh -L` tunnels) from
  `on_project_start`** — it runs in the launching shell and *blocks* session
  creation until it returns, so the session would hang forever. Use launchd
  for anything persistent.
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
- Optional: shell aliases in `~/.zshrc` (`alias mux='tmuxinator'`,
  `alias mw='tmuxinator start work'`, `alias mi='tmuxinator start ide'`).
