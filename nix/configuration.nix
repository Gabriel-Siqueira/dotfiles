{ lib, pkgs, config, modulesPath, ... }:

with lib;
{
  imports = [
    "${modulesPath}/profiles/minimal.nix"
  ];

  wsl = {
    enable = true;
    wslConf.automount.root = "/mnt";
    defaultUser = "gabriel";
    startMenuLaunchers = true;

    # Enable native Docker support
    # docker-native.enable = true;

    # Enable integration with Docker Desktop (needs to be installed)
    # docker-desktop.enable = true;

  };

  users.users.gabriel = {
    isNormalUser  = true;
    home  = "/home/gabriel";
    extraGroups  = [ "wheel" "networkmanager" ];
  };

  networking.hostName = "GERy";

  # Enable nix flakes
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  environment.variables.EDITOR = "nvim";
  environment.pathsToLink = [ "/share/zsh" ];

  system.stateVersion = "23.05";
}
