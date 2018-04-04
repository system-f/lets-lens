{ mkDerivation, base, containers, directory, doctest, filepath
, QuickCheck, stdenv, template-haskell
}:
mkDerivation {
  pname = "lets-lens";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base containers ];
  testHaskellDepends = [
    base directory doctest filepath QuickCheck template-haskell
  ];
  homepage = "https://github.com/data61/lets-lens";
  description = "Source code for exercises on the lens concept";
  license = stdenv.lib.licenses.bsd3;
}
