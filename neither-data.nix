{ mkDerivation, base, lib }:
mkDerivation {
  pname = "neither-data";
  version = "0.2.1.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/schuelermine/neither-data";
  description = "The Neither datatype";
  license = lib.licenses.mit;
}
