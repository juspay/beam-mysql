{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/89c2b2330e733d6cdb5eae7b899326930c2c0648";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    bytestring-lexing.url = "github:juspay/bytestring-lexing";
    bytestring-lexing.flake = false;

    ## Use Juspay upstream after PR merged here https://github.com/juspay/mysql-haskell/pull/5
    mysql-haskell.url = "github:infinitumkiran/mysql-haskell?ref=ghc984";
    mysql-haskell.inputs.nixpkgs.follows = "nixpkgs";
    mysql-haskell.inputs.haskell-flake.follows = "haskell-flake";

    ## Use Juspay upstream after PR merged - https://github.com/juspay/beam/pull/21
    beam.url = "github:infinitumkiran/beam?ref=ghc98";
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
          basePackages = pkgs.haskell.packages.ghc98;
          imports = [
            inputs.beam.haskellFlakeProjectModules.output
            inputs.mysql-haskell.haskellFlakeProjectModules.output
          ];

          packages = {
            record-dot-preprocessor.source ="0.2.17";
            quickcheck.source="2.14.3";
            bytestring-lexing.source = inputs.bytestring-lexing;
            mysql-haskell.source = inputs.mysql-haskell;
            beam.source = inputs.beam; 
            regex-tdfa.source="1.3.2.5";
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
            int-cast = {
              broken = false;
              jailbreak = true;
            };

          };
          autoWire = [ "packages" "checks" "devShells" "apps"];
        };
      };
    });
}
