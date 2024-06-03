{ pkgs, modulesPath, lib, overlays, ... }:

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
  };

  environment = {
    noXlibs = lib.mkForce false; # openjdk does not install without this
    variables.EDITOR = "nvim";
    pathsToLink = [ "/share/zsh" ];
  };

  users.users.gabriel = {
    isNormalUser = true;
    home = "/home/gabriel";
    extraGroups = [ "wheel" "networkmanager" ];
  };

  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  networking.hostName = "GERy";

  # Enable nix flakes
  nix.package = pkgs.nixVersions.latest;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
    inherit overlays;
  };

  system.stateVersion = "23.05";
}
