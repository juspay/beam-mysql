{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    bytestring-lexing.url = "github:juspay/bytestring-lexing";
    bytestring-lexing.flake = false;

    mysql-haskell.url = "github:juspay/mysql-haskell";
    mysql-haskell.inputs.nixpkgs.follows = "nixpkgs";
    mysql-haskell.inputs.haskell-flake.follows = "haskell-flake";

    beam.url = "github:juspay/beam";
    beam.inputs.haskell-flake.follows = "haskell-flake";
    beam.inputs.nixpkgs.follows = "nixpkgs";

  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', pkgs, lib, config, ... }: {
        haskellProjects.default = {
          projectFlakeName = "beam-mysql";
          basePackages = pkgs.haskell.packages.ghc927;
          imports = [
            inputs.beam.haskellFlakeProjectModules.output
            inputs.mysql-haskell.haskellFlakeProjectModules.output
          ];

          packages = {
            bytestring-lexing.source = inputs.bytestring-lexing;
          };

          settings = {

            bytestring-lexing = {
              jailbreak = true;
            };

            mysql-haskell = {
             check = false;
            };

            beam-mysql = {
              jailbreak = true;
            };

          };
          autoWire = [ "packages" "checks" "devShells" "apps"];
        };
      };
    });
}
