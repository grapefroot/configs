# home-manager configuration for the MacBook (mirrors modules/home.nix shape).
#
# Reuses ./programs/zsh.nix so the Mac's shell is identical to the
# workstation's (oh-my-zsh, lambda theme, syntax highlighting,
# autosuggestions, 10k history). The packages that used to live in the
# Mac's environment.systemPackages now live in the home-manager user
# profile instead.

{ pkgs, ... }:

{
  home-manager.useUserPackages = true;
  home-manager.backupFileExtension = "hm-backup";

  home-manager.users.grapefroot = { pkgs, ... }: {
    imports = [
      ./programs/zsh.nix
    ];

    home.packages = with pkgs; [
      # --- CLI utilities ---
      bat
      fd
      ripgrep
      jq
      tmux
      tmuxinator
      gh
      jless
      television

      # --- Languages / dev tools ---
      go
      nodejs_24
      uv
      cmake
      bison
      doxygen

      # --- Media ---
      ffmpeg_7-headless
      gallery-dl
      yt-dlp
      hugo

      # --- Cloud / containers ---
      awscli2
      s3cmd
      docker

      # --- Docs ---
      asciidoc
      docutils

      # --- Libraries (kept for dev/linking, as in the old profile) ---
      openssl
      zlib
      libpng
      pcre2
      xz
      gobject-introspection
    ];

    home.username = "grapefroot";
    home.homeDirectory = "/Users/grapefroot";
    home.stateVersion = "26.05";
  };
}
