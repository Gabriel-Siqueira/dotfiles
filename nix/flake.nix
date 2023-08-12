{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    NixOS-WSL = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixneovimplugins.url = "github:jooooscha/nixpkgs-vim-extra-plugins";
  };

  outputs = { self, nixpkgs, NixOS-WSL, home-manager, nixneovimplugins }:
  let
    username = "gabriel";
    system = "x86_64-linux";
    stateVersion = "23.05";
    pkgs = import nixpkgs { inherit system; };

    overlays = [
      (final: prev: {
        homeDirectory = "/home/${username}";
        inherit stateVersion username;
      })
      nixneovimplugins.overlays.default
    ];
  in
  {
    nixosConfigurations."GERy" = nixpkgs.lib.nixosSystem {
      inherit system;

      modules = [
        self.localModules.base
        NixOS-WSL.nixosModules.wsl
	home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.gabriel = import ./home.nix;
        }
      ];
    };

    localModules = {
        base = { pkgs, lib, modulesPath,... }: import ./configuration.nix {
          inherit overlays pkgs lib modulesPath;
        };
    };
  };
}
