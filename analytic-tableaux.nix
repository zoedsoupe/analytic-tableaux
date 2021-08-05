{ mkDerivation, base, lib }:

mkDerivation {
  pname = "analytic-tableaux";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
}
