{ mkDerivation, base, lib }:
mkDerivation {
  pname = "neither-data";
  version = "0.2.3.4";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/schuelermine/neither-data";
  description = "The Neither datatype";
  license = lib.licenses.mit;
}
