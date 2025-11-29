{
  description = "Standalone Magit with Evil (Vim keybindings) support";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        magit-standalone = pkgs.callPackage ./package.nix { };

        treefmtEval = treefmt-nix.lib.evalModule pkgs {
          projectRootFile = "flake.nix";
          programs.nixfmt.enable = true;
          programs.prettier.enable = true;
        };
      in
      {
        formatter = treefmtEval.config.build.wrapper;
        packages = {
          inherit magit-standalone;
          default = magit-standalone;
        };

        apps = {
          default = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/magit";
          };
          magit-standalone = {
            type = "app";
            program = "${self.packages.${system}.magit-standalone}/bin/magit";
          };
        };

        checks = {
          formatting = treefmtEval.config.build.check self;
        };
      }
    );
}
