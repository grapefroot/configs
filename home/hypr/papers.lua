-- papers Hyprland profile (DESIGN.md §5)
-- Stripped per §4/§6: NO exec / close / exit / launcher binds. Apps launch
-- only via the autostart below; close a window and it stays closed until
-- relogin (no self-healing, by design).
--
-- Three workspaces: sioyek (PDF) / zed (notes) / foot (terminal).
-- Look & feel + navigation ported from grapefroot's ~/.config/hypr/hyprland.lua.

------------------
---- MONITORS ----
------------------

hl.monitor({
    output   = "",
    mode     = "preferred",
    position = "auto",
    scale    = "1",
})

---------------------
---- MY PROGRAMS ----
---------------------

local terminal = "foot"

-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------

hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")
-- Sioyek is Qt; run natively on Wayland. Remove this line if sioyek misbehaves.
hl.env("QT_QPA_PLATFORM", "wayland")

-----------------------
---- LOOK AND FEEL ----
-----------------------

hl.config({
    general = {
        gaps_in  = 0,
        gaps_out = 0,

        border_size = 1,

        col = {
            active_border   = "rgba(aa0000ff)",  -- xmonad myFocusedBorderColor #aa0000
            inactive_border = "rgba(222222ff)",  -- xmonad myNormalBorderColor  #222222
        },

        resize_on_border = false,
        allow_tearing    = false,
        layout           = "dwindle",
    },

    decoration = {
        rounding       = 0,
        rounding_power = 0,

        active_opacity   = 1.0,
        inactive_opacity = 1.0,

        shadow = {
            enabled      = false,
            range        = 4,
            render_power = 3,
            color        = 0xee1a1a1a,
        },

        blur = {
            enabled  = false,
            size     = 3,
            passes   = 1,
            vibrancy = 0.1696,
        },
    },

    animations = { enabled = false },

    dwindle = {
        preserve_split  = true,
        force_split     = 2,
        smart_resizing  = false,
    },
})

----------------
----  MISC  ----
----------------

hl.config({
    misc = {
        force_default_wallpaper  = 0,
        disable_hyprland_logo    = true,
        disable_splash_rendering = true,
    },
})

---------------
---- INPUT ----
---------------

hl.config({
    input = {
        kb_layout  = "us",
        kb_variant = "",
        kb_model   = "",
        kb_options = "",
        kb_rules   = "",

        follow_mouse = 1,
        sensitivity  = 0,

        touchpad = { natural_scroll = false },
    },
})

hl.gesture({
    fingers    = 3,
    direction  = "horizontal",
    action     = "workspace",
})

---------------------
---- KEYBINDINGS ----
---------------------
-- Stripped: no terminal/close/exit/launcher/filemanager/lock binds.
-- Only layout manipulation, focus, workspace switch, and hardware keys.

local mainMod = "SUPER"

-- Promote focused window to root of workspace tree (xmonad swapMaster-ish)
hl.bind(mainMod .. " + Return", hl.dsp.layout("movetoroot active"))

-- Resize (xmonad Shrink / Expand)
hl.bind(mainMod .. " + h", hl.dsp.window.resize({ x = -20, y = 0, relative = true }), { repeating = true })
hl.bind(mainMod .. " + l", hl.dsp.window.resize({ x =  20, y = 0, relative = true }), { repeating = true })

-- Cycle focus (xmonad focusUp / focusDown)
hl.bind(mainMod .. " + k", hl.dsp.window.cycle_next({ direction = "next" }))
hl.bind(mainMod .. " + j", hl.dsp.window.cycle_next({ direction = "prev" }))

-- Swap focused window with neighbour (xmonad swapUp / swapDown)
hl.bind(mainMod .. " + SHIFT + j", hl.dsp.window.swap({ prev = true }))
hl.bind(mainMod .. " + SHIFT + k", hl.dsp.window.swap({ next = true }))

-- Tiling manipulation
hl.bind(mainMod .. " + V", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + R", hl.dsp.window.pseudo())

-- Workspaces 1..3: sioyek / zed / foot
for i = 1, 3 do
    local key = i % 10 -- 10 maps to key 0 (unused here, but keeps the pattern)
    hl.bind(mainMod .. " + " .. key,         hl.dsp.focus({ workspace = i }))
    hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end

-- Scratchpad
hl.bind(mainMod .. " + S",         hl.dsp.workspace.toggle_special("magic"))
hl.bind(mainMod .. " + SHIFT + S", hl.dsp.window.move({ workspace = "special:magic" }))

-- Workspace scroll
hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up",   hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- Hardware keys (volume via system-wide wpctl; brightness needs brightnessctl in closure)
hl.bind("XF86AudioRaiseVolume",  hl.dsp.exec_cmd("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume",  hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"),      { locked = true, repeating = true })
hl.bind("XF86AudioMute",         hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),     { locked = true, repeating = true })
hl.bind("XF86AudioMicMute",      hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"),   { locked = true, repeating = true })
hl.bind("XF86MonBrightnessUp",   hl.dsp.exec_cmd("brightnessctl -e4 -n2 set 5%+"),                  { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl -e4 -n2 set 5%-"),                  { locked = true, repeating = true })

--------------------------------
---- WINDOWS AND WORKSPACES ----
--------------------------------

-- Pin apps to workspaces. WM_CLASS verified for sioyek (StartupWMClass=sioyek)
-- and zed (dev.zed.Zed). If an app lands on the wrong workspace, check
-- `hyprctl clients | grep class` and fix the regex below.

hl.window_rule({
    name      = "pin-sioyek-ws1",
    match     = { class = "^sioyek$" },
    workspace = "1 silent",
})

hl.window_rule({
    name      = "pin-zed-ws2",
    match     = { class = "^dev\\.zed\\.Zed$" },
    workspace = "2 silent",
})

hl.window_rule({
    name      = "pin-foot-ws3",
    match     = { class = "^foot$" },
    workspace = "3 silent",
})

-- Autostart the three apps at login (DESIGN.md §6: no self-healing)
hl.on("hyprland.start", function ()
    hl.exec_cmd("sioyek")        -- → ws1
    hl.exec_cmd("zeditor")       -- dev.zed.Zed → ws2
    hl.exec_cmd(terminal)        -- foot → ws3
end)
