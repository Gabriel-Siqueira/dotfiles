{ pkgs, modulesPath, lib, overlays, ... }:

with lib;
{
  imports = [
    ./hardware-configuration.nix
  ];

  # Select specific kernel if current kernel is not working.
  # boot.kernelPackages = pkgs.linuxPackages_6_10;

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.configurationLimit = 3;

  networking.hostName = "GABe"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "pt_BR.UTF-8";
    LC_IDENTIFICATION = "pt_BR.UTF-8";
    LC_MEASUREMENT = "pt_BR.UTF-8";
    LC_MONETARY = "pt_BR.UTF-8";
    LC_NAME = "pt_BR.UTF-8";
    LC_NUMERIC = "pt_BR.UTF-8";
    LC_PAPER = "pt_BR.UTF-8";
    LC_TELEPHONE = "pt_BR.UTF-8";
    LC_TIME = "pt_BR.UTF-8";
  };

  # environment.systemPackages = with pkgs; [
  # pkgs.fprintd
  # ];


  services = {
    # Enable the KDE Plasma Desktop Environment.
    desktopManager.plasma6.enable = true;

    udev.extraRules = builtins.readFile ./udev.rules;

    displayManager = {
      sddm.enable = true;
      sddm.wayland.enable = true;
      # Enable automatic login for the user.
      autoLogin.enable = true;
      autoLogin.user = "gabriel";
    };

    xserver = {
      # Enable the X11 windowing system.
      enable = true;

      # Configure keymap in X11
      xkb = {
        layout = "us";
        variant = "";
        options = "compose:ralt, caps:escape";
      };
    };

    # Fingerprint reader
    # fprintd = {
    #   enable = true;
    #   tod.enable = true;
    #   tod.driver = pkgs.libfprint-2-tod1-vfs0090;
    # };

    # Enable CUPS to print documents.
    printing.enable = true;
  };

  # Allow ports for dropbox
  networking.firewall = {
    allowedTCPPorts = [ 17500 ];
    allowedUDPPorts = [ 17500 ];
  };

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Bluetooth
  hardware = {
    bluetooth = {
        enable = true; # enables support for Bluetooth
        powerOnBoot = true; # powers up the default Bluetooth controller on boot
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.gabriel = {
    isNormalUser = true;
    description = "Gabriel";
    home = "/home/gabriel";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  environment = {
    variables.EDITOR = "nvim";
    pathsToLink = [ "/share/zsh" ];
  };

  programs.zsh.enable = true;
  programs.steam.enable = true;
  users.defaultUserShell = pkgs.zsh;

  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
    polkitPolicyOwners = [ "gabriel" ];
  };

  fonts.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" ]; })
  ];

  # Enable nix flakes
  nix.package = pkgs.nixVersions.latest;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  # Alternative cache
  nix.settings.substituters = [ "https://aseipp-nix-cache.global.ssl.fastly.net" ];

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
    inherit overlays;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}
