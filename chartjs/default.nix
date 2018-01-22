{ reflex-platform ? import ./deps/reflex-platform {}
, ghcjs ? reflex-platform.ghcjs }:

let
  pkgs = import <nixpkgs> { };

  chartjs = ghcjs.callPackage ./packages.nix {
  };

  dontHaddockPackages = [
    "ghcjs"
    "reflex-platform"
  ];

in chartjs
