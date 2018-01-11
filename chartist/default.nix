{ reflex-platform ? import ./deps/reflex-platform {}
, ghcjs ? reflex-platform.ghcjs }:

let
  pkgs = import <nixpkgs> { };

  chartist = ghcjs.callPackage ./packages.nix {
  };

  dontHaddockPackages = [
    "ghcjs"
    "reflex-platform"
  ];

in chartist
