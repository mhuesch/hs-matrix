{ mkDerivation, base, lib, microlens, microlens-th, MonadRandom
, primitive, random, terminal-size, vector, vty
}:
mkDerivation {
  pname = "hs-matrix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base microlens microlens-th MonadRandom primitive random
    terminal-size vector vty
  ];
  license = lib.licenses.gpl3Only;
  mainProgram = "hmx";
}
