# keyd: key remapping daemon (https://github.com/rvaiya/keyd).
# Operates at the kernel/evdev level via /dev/uinput, so remaps apply in
# Wayland, X11 and the TTY alike. See `man keyd`.
{ ... }:

{
  services.keyd = {
    enable = true;

    keyboards.externalKeyboard = {
      # Lite-On Tech Lenovo USB Travel Keyboard with Ultra Nav
      # (vendor:product from /proc/bus/input/devices). Scoping to this id
      # leaves the built-in ThinkPad keyboard untouched.
      #
      # The Lenovo USB device exposes several interfaces under the same
      # vendor:product (17ef:6080): three keyboard interfaces and one
      # trackpoint/mouse interface ("Ultra Nav Mouse", /dev/input/event6).
      # keyd matches by vendor:product, so a bare `17ef:6080` (or `k:` --
      # keyd treats mouse buttons as EV_KEY, so `k:` does NOT exclude it)
      # grabs event6 too. That exclusive grab blocks Hyprland/libinput from
      # reading the trackpoint directly, breaking middle-button scrolling
      # (see modules/libinput-quirks.nix).
      #
      # Fix: exclude the mouse interface by its full id. keyd's device id is
      # `<vendor>:<product>:<hash>` where <hash> = generate_uid() -- a djb2
      # hash of stable device properties (key count, abs/rel masks, name),
      # so it is stable across replugs/reboots. From `journalctl -u keyd`,
      # the mouse interface's id is 17ef:6080:3f467f11.
      #
      # keyd's config_check_match() iterates ids in list order and on a
      # prefix match returns immediately: ID_EXCLUDED -> 0 (skip), positive
      # -> match. Listing the exclude first means event6 is skipped while
      # the three keyboard interfaces still match `17ef:6080` and keep the
      # remap below. Verified against keyd 2.6.0 source (src/config.c).
      ids = [
        "-17ef:6080:3f467f11"   # exclude trackpoint/mouse interface (event6)
        "17ef:6080"             # match the three keyboard interfaces
      ];

      settings = {
        # ISO "<" key = the non-US backslash key (KEY_102ND) -- the key next
        # to left Shift on ISO layouts. Swap roles with left Alt so that:
        #   <        -> left Alt
        #   left Alt -> left Super ("Windows"/"Command")
        # The keyboard has no Windows key, so left Alt takes that role and
        # `<` picks up the freed Alt.
        main = {
          "102nd" = "leftalt";
          leftalt = "leftmeta";
        };
      };
    };
  };
}
