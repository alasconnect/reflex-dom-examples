{ reflex-platform ? import ./deps/reflex-platform {}
, ghcjs ? reflex-platform.ghcjs }:

let
  pkgs = import <nixpkgs> { };

  plotly = ghcjs.callPackage ./packages.nix {
  };

  dontHaddockPackages = [
    "ghcjs"
    "reflex-platform"
  ];

in plotly
