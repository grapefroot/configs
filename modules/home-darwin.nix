# macOS Home Manager configuration; shares shell/editor settings with Linux.
{ ... }:
{
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;
  home-manager.backupFileExtension = "hm-backup";

  home-manager.users.grapefroot = { pkgs, ... }: {
    imports = [
      ./home/common.nix
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
      gopls
      nodejs_24
      pyright
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
      tailscale

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
