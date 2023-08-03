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
    # TODO add python script file-watcher here (optional emacs after-save hook)
    pkgs.syncthing
    pkgs.ripgrep
    pkgs.ruff
    pkgs.nodePackages.pyright
  ];

  # I keep emacs dotfiles symlinked to edit .emacs.d and don't have to reload
  # home-manager config
  # TODO user and system agnostic naming

  home.file = {
    ".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/.emacs.d";
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/puhsu/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    EDITOR = "emacs";
  };

  # TODO think about modularizing the config later and loading modules
  # Probably to early for this

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
    # Moved packages to manual install for now
    extraPackages = epkgs: [
      epkgs.vertico
      epkgs.orderless
      epkgs.marginalia
      epkgs.consult
      epkgs.embark
      epkgs.corfu
      epkgs.cape
      epkgs.magit
      epkgs.modus-themes
      epkgs.nix-mode
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
