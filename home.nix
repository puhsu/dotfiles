
{ config, pkgs, lib, ... }:
{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  
  # This is in flake.nix for now, TODO, find a cleaner way to share the same home.nix between different machines and users
  # home.username = (builtins.getEnv "USER");
  # home.homeDirectory = (/. + builtins.getEnv "HOME");
  home.username = "irubachev";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    pkgs.micromamba  # when in teams, hard to work with nix python :tear_emojy:
    pkgs.unison 
    pkgs.jq
    # TODO add python script file-watcher here (optional: emacs after-save hook)
    pkgs.geckodriver
    pkgs.typst
    pkgs.llama-cpp
    pkgs.ollama
    pkgs.tdlib
    pkgs.rustup # TODO only for the unison synchronizer
    pkgs.p7zip
    pkgs.yt-dlp
    pkgs.ripgrep
    pkgs.fd
    pkgs.ruff
    pkgs.ffmpeg
    pkgs.aria
    pkgs.wget
    pkgs.yarn
    pkgs.nodePackages.pyright
    pkgs.nodePackages.pnpm
    pkgs.texlive.combined.scheme-minimal
    pkgs.python311Packages.ruff-lsp
  ];

  # I keep emacs dotfiles symlinked to edit .emacs.d and don't have to reload
  # home-manager config

  home.file = {
    ".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/.emacs.d";
    ".profile".text = ''XDG_RUNTIME_DIR=$(mktemp -d) exec "~/.nix-profile/bin/fish"'';
  };

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  # TODO think about modularizing the config later and loading modules
  # Probably to early for this

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
    extraPackages = epkgs: [
      epkgs.vertico
      epkgs.orderless
      epkgs.marginalia
      epkgs.consult
      epkgs.embark
      epkgs.embark-consult
      epkgs.wgrep
      epkgs.corfu
      epkgs.cape
      epkgs.magit
      epkgs.lsp-mode
      epkgs.lsp-pyright
      epkgs.eglot
      epkgs.gptel
      epkgs.consult-lsp
      epkgs.jupyter
      epkgs.flycheck
      epkgs.gcmh
      epkgs.ctrlf
      epkgs.svg-lib
      epkgs.modus-themes
      epkgs.nano-modeline
      epkgs.nix-mode
      epkgs.org-roam
      epkgs.org-noter
      epkgs.org-pdftools
      epkgs.pdf-tools
      epkgs.helpful
    ];
  };

  # Do not create symlinks for applications in ~/Applications
  disabledModules = [ "targets/darwin/linkapps.nix" ];

  # Manually copy the Emacs.app
  home.activation = lib.mkIf pkgs.stdenv.isDarwin {
    copyApplications = let
      apps = pkgs.buildEnv {
        name = "home-manager-applications";
        paths = config.home.packages;
        pathsToLink = "/Applications";
      };
    in lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      baseDir="$HOME/Applications/Home Manager Apps"
      if [ -d "$baseDir" ]; then
        rm -rf "$baseDir"
      fi
      mkdir -p "$baseDir"
      for appFile in ${apps}/Applications/*; do
        target="$baseDir/$(basename "$appFile")"
        $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
        $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
      done
    '';
  };

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
        # >>> mamba initialize >>>
        set -gx MAMBA_EXE "${config.home.homeDirectory}/.nix-profile/bin/micromamba"
        set -gx MAMBA_ROOT_PREFIX "${config.home.homeDirectory}/micromamba"
        $MAMBA_EXE shell hook --shell fish --prefix $MAMBA_ROOT_PREFIX | source
        # <<< mamba initialize <<<
    '';

    plugins = [
      # Need this when using Fish as a default macOS shell in order to pick
      # up ~/.nix-profile/bin
      {
        name = "nix-env";
        src = pkgs.fetchFromGitHub {
          owner = "lilyball";
          repo = "nix-env.fish";
          rev = "7b65bd228429e852c8fdfa07601159130a818cfa";
          sha256 = "069ybzdj29s320wzdyxqjhmpm9ir5815yx6n522adav0z2nz8vs4";
        };
      }
    ];
  };

  programs.git = {
    enable = true;
    userEmail = "irubachev@gmail.com";
    userName = "irubachev";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
