{ reflex-platform ? import ./deps/reflex-platform {}
, ghcjs ? reflex-platform.ghcjs }:

let
  pkgs = import <nixpkgs> { };

  rfex-base = ghcjs.callPackage ./packages.nix {
  };

  dontHaddockPackages = [
    "ghcjs"
    "reflex-platform"
  ];

in rfex-base
