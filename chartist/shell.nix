{ reflex-platform ? import ../submodules/reflex-platform {}
, ghcjs ? reflex-platform.ghcjs }:

let
  drv = import ./default.nix {
    inherit reflex-platform;
    inherit ghcjs;
  };
in drv.env
