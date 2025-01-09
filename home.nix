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
    pkgs.uv
    pkgs.devenv # pixi from the nix world (still no idea why I need this)
    pkgs.cachix
    pkgs.zig
    pkgs.zls
    
    pkgs.ncdu

    # for remote development
    pkgs.unison
    pkgs.syncthing
    pkgs.ghostscript
    pkgs.jq
    # pkgs.uv
    
    # pkgs.espeak # for local tts
    pkgs.cloc # count lines of code
    pkgs.exercism
    pkgs.eternal-terminal
    pkgs.jujutsu
    
    # spell checking lib
    pkgs.enchant

    # TODO add python script file-watcher here (optional: emacs after-save hook)
    pkgs.geckodriver
    pkgs.git-lfs
    
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
    pkgs.pyright
    pkgs.nodePackages.pnpm
    # pkgs.texliveMedium
    pkgs.doxygen
    pkgs.typst
  ] ++ lib.optionals pkgs.stdenv.isDarwin [
    pkgs.aerospace
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

  programs.ghostty = pkgs.lib.mkIf pkgs.stdenv.isDarwin {
    package = (pkgs.callPackage ./pkgs/ghostty.nix {});
    enable = true;

    settings = {
      command = (lib.getExe pkgs.fish);
      theme = "BlulocoLight";
      
      font-family = "PragmataPro Mono Liga";
      cursor-style = "block";
      scrollback-limit = 2 * 1024 * 1024 * 1024; # 2GB 
      title = " ";
      shell-integration = "fish";
      copy-on-select = true;
      macos-option-as-alt = true;
      macos-titlebar-style = "tabs";
      macos-icon = "custom-style";
    };
  };
    

  # TODO think about modularizing the config later and loading modules
  # Probably to early for this

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-git;
    extraPackages = epkgs: [
      epkgs.vertico
      epkgs.eat
      epkgs.jinx
      epkgs.orderless
      epkgs.marginalia
      epkgs.consult
      epkgs.vundo
      epkgs.embark
      epkgs.embark-consult
      epkgs.jupyter
      epkgs.helpful
      epkgs.elisp-demos
      epkgs.visual-fill-column
      epkgs.which-key
      epkgs.wgrep
      epkgs.corfu
      epkgs.cape
      epkgs.magit
      epkgs.gptel
      epkgs.consult-lsp
      epkgs.ctrlf
      epkgs.visual-replace
      epkgs.svg-lib
      epkgs.modus-themes
      epkgs.spacious-padding

      epkgs.nix-mode
      epkgs.denote
      epkgs.dwim-shell-command
      epkgs.nongnuPackages.zig-mode
      epkgs.eglot

      # todo: learn how to move this stuff to modules
      (epkgs.melpaBuild {
        pname = "inline-diff";
        version = "0.0.1";
        recipe = pkgs.writeText "recipe" ''
          (inline-diff
            :url "https://code.tecosaur.net/tec/inline-diff.git"
            :fetcher git)
        '';
        commit = "5ee85842d230d07b31413ddf0ec610b306f1be37";
        src = builtins.fetchGit {
          url = "https://code.tecosaur.net/tec/inline-diff.git";
          rev = "5ee85842d230d07b31413ddf0ec610b306f1be37";          
        };
        meta = {
          description = "Diff in-place";
        };
      })

      (epkgs.melpaBuild {
        pname = "ultra-scroll";
        version = "0.0.1";
        recipe = pkgs.writeText "recipe" ''
          (ultra-scroll
            :repo "jdtsmith/ultra-scroll"
            :fetcher github)
        '';
        commit = "e89d15e18549bc6b3c41f21c3cf5082b04fea303";
        src = pkgs.fetchFromGitHub {
          owner = "jdtsmith";
          repo = "ultra-scroll";
          rev = "e89d15e18549bc6b3c41f21c3cf5082b04fea303";
          sha256 = "sha256-XRaBUu1kJbuqkyhiyko22c+ZORCLu5zvAazrVeM51eY=";
        };
        meta = {
          description = "Scroll emacs like lightning";
        };
      })

    ];
  };

  # Manually copy the *.app instead of symlinking them
  # This is a hack that can actually brake some app's (e.g. the ghostty app)

  disabledModules = [ "targets/darwin/linkapps.nix" ];
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
      for appFile in ${apps}/Applications/*.app; do
        target="$baseDir/$(basename "$appFile")"
        $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
        $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
      done

      # This is restarted (wait till ghostty comes around o
      for dmgFile in ${apps}/Applications/*.dmg; do
        target="$baseDir/$(basename "$dmgFile" ".dmg").app"
        mount_point=$baseDir"/$(basename "$dmgFile" ".dmg")"

        # Attach and extract dmg
        if [ -n "$VERBOSE_ARG" ]; then
          $DRY_RUN_CMD /usr/bin/hdiutil attach "$dmgFile" -nobrowse -readonly -mountpoint "$mount_point"
        else
          $DRY_RUN_CMD /usr/bin/hdiutil attach "$dmgFile" -nobrowse -readonly -mountpoint "$mount_point" > /dev/null 2>&1
        fi

        for appFile in "$mount_point"/*.app; do
          $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -pR "$appFile" "$baseDir"
        done

        # Detach dmg
        if [ -n "$VERBOSE_ARG" ]; then
          $DRY_RUN_CMD /usr/bin/hdiutil detach "$mount_point" -force
        else
          $DRY_RUN_CMD /usr/bin/hdiutil detach "$mount_point" -force > /dev/null 2>&1
        fi
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

        # disable hello from fish
        set fish_greeting
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
