{ mkDerivation, aeson, base, bytestring, containers, ghcjs-base
, ghcjs-dom, lens, reflex, reflex-dom, stdenv, text, time
}:
mkDerivation {
  pname = "chartjs";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers ghcjs-base ghcjs-dom lens reflex
    reflex-dom text time
  ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.unfree;
}
