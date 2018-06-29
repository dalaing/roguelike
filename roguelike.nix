{ mkDerivation, base, brick, containers, lens, linear, mtl, random
, reflex, reflex-brick, stdenv, vty
}:
mkDerivation {
  pname = "roguelike";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick containers lens linear mtl random reflex reflex-brick
    vty
  ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
