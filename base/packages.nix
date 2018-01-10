{ mkDerivation, base, containers, ghcjs-dom, lens, reflex
, reflex-dom, stdenv, text, time
}:
mkDerivation {
  pname = "rfex-base";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers ghcjs-dom lens reflex reflex-dom text time
  ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.unfree;
}
