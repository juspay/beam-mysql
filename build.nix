{ devShell ? false, inCI ? false }:
let
  flake-compat = fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/19576c2aea7f074ff0da818b21a8b0950ff6ec86.tar.gz";
    sha256 = "13jf267qvd4fvph27flp5slrn6w2q26mhpakr8bj2ppqgyjamb79";
  };

  nix-inclusive = fetchTarball {
    url = "https://github.com/juspay/nix-inclusive/archive/2ca1706029bfcf4bb7eaf17b4f32e49f436a148e.tar.gz";
    sha256 = "1y3vhqnbh5kg906fpw22h670ppl8238xwv0dx7zdcp22212zdjnx";
  };

  filter = import "${nix-inclusive}/inclusive.nix" { lib = (import <nixpkgs> {}).lib; };
  path =
    filter ./. [
      ./flake.nix
      ./flake.lock
      ./nix/overlay.nix
      ./src
      ./test
      ./db
      ./pool
      ./beam-mysql.cabal
      ./LICENSE
    ];
  attr = if devShell then "devShell" else "defaultPackage";
  compat-attr = if devShell then "shellNix" else "defaultNix";

  flake-drv =
    if inCI
    then (import flake-compat { src = path; }).${compat-attr}
    else (builtins.getFlake (toString (builtins.unsafeDiscardStringContext path)));
in
flake-drv.${attr}.${builtins.currentSystem}
