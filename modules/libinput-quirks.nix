# libinput device quirks: /etc/libinput/local-overrides.quirks
# See https://wayland.freedesktop.org/libinput/doc/latest/quirks.html
#
# libinput loads its quirk database once, when the compositor creates its
# libinput context (at login). Editing this file then requires a relogin
# (or Hyprland restart) to take effect -- replugging the device is not
# enough, since the cached database is consulted for device-add events.
{ ... }:

{
  environment.etc."libinput/local-overrides.quirks".text = ''
    # Tag the Lenovo USB Travel Keyboard's trackpoint ("Ultra Nav Mouse"
    # interface, /dev/input/event6) as a pointing stick.
    #
    # The hardware does not set INPUT_PROP_POINTING_STICK (kernel
    # /sys/.../properties reads 0, vs 0x21 for the built-in TPPS/2 Elan
    # TrackPoint), so libinput classifies it as a plain mouse and leaves
    # on-button scrolling disabled by default. Per libinput's docs:
    # https://wayland.freedesktop.org/libinput/doc/latest/scrolling.html#on-button-scrolling
    # on-button scrolling is enabled by default *only* for pointing sticks.
    #
    # Adding the property here makes libinput treat this device like the
    # built-in trackpoint: holding the middle button converts trackpoint
    # motion into scroll events (instead of falling through to a middle
    # click / X11 selection paste).
    #
    # Match attributes from `udevadm info /dev/input/event6`:
    #   ID_BUS=usb  ID_VENDOR_ID=17ef  ID_MODEL_ID=6080
    # MatchUdevType=mouse scopes this to the mouse/trackpoint interface
    # (interface 01); the keyboard interface (00) shares the same
    # vendor:product but has ID_INPUT_KEYBOARD, so it won't match.
    [Lenovo USB Travel Keyboard with Ultra Nav Trackpoint]
    MatchUdevType=mouse
    MatchBus=usb
    MatchVendor=0x17EF
    MatchProduct=0x6080
    AttrInputProp=+INPUT_PROP_POINTING_STICK
  '';
}
