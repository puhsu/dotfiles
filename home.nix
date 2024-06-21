
{config, pkgs, lib, ... }:
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
    # for remote development
    pkgs.unison
    pkgs.autossh
    pkgs.jq
    pkgs.dua  # disk space monitor
    pkgs.uv
    
    pkgs.espeak # for local tts
    pkgs.cloc # count lines of code
    pkgs.exercism
    pkgs.eternal-terminal
    # TODO add python script file-watcher here (optional: emacs after-save hook)
    pkgs.geckodriver
    pkgs.git-lfs
    pkgs.typst
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
    pkgs.emacs-lsp-booster
    pkgs.nodePackages.pnpm
    pkgs.texliveFull
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
    package = pkgs.emacs;
    extraPackages = epkgs: [
      epkgs.vertico
      epkgs.eat
      epkgs.orderless
      epkgs.marginalia
      epkgs.consult
      epkgs.vundo
      epkgs.embark
      epkgs.jupyter
      epkgs.embark-consult
      epkgs.helpful
      epkgs.elisp-demos
      epkgs.visual-fill-column
      epkgs.which-key
      epkgs.wgrep
      epkgs.corfu
      epkgs.cape
      epkgs.magit

      # IDE features (eglot for completion and references
      epkgs.eglot

      # This would be obsolete in emacs 30 once faster json parser arive
      (epkgs.melpaBuild {
        pname = "eglot-booster";
        version = "0.0.1";
        recipe = pkgs.writeText "recipe" ''
          (eglot-booster
            :repo "jdtsmith/eglot-booster"
            :fetcher github)
        '';
        commit = "e19dd7ea81bada84c66e8bdd121408d9c0761fe6";
        src = pkgs.fetchFromGitHub {
          owner = "jdtsmith";
          repo = "eglot-booster";
          rev = "e19dd7ea81bada84c66e8bdd121408d9c0761fe6";
          sha256 = "sha256-vF34ZoUUj8RENyH9OeKGSPk34G6KXZhEZozQKEcRNhs";
        };
        meta = {
          description = "Eglot Integration with emacs-lsp-booster";
        };
      })
      epkgs.gptel
      epkgs.consult-lsp
      
      epkgs.ctrlf
      epkgs.svg-lib
      epkgs.modus-themes

      epkgs.nix-mode
      epkgs.org-roam
      epkgs.org-noter
      epkgs.org-pdftools
      epkgs.pdf-tools
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

        set -gx LANG en_US.UTF-8
        set -gx LC_ALL en_US.UTF-8
    '';

    plugins = [
      # Need this when using Fish as a default macOS shell in order to pick
      # up ~/.nix-profile/bin in PATH
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
