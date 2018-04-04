{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  tasty-hedgehog-github = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "qfpl";
    repo = "tasty-hedgehog";
    rev = "5da389f5534943b430300a213c5ffb5d0e13459e";
    sha256 = "04pmr9q70gakd327sywpxr7qp8jnl3b0y2sqxxxcj6zj2q45q38m";
  }) {};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      tasty-hedgehog =
        if super ? tasty-hedgehog
        then super.tasty-hedgehog
        else tasty-hedgehog-github;
    };
  };

  lets-lens = modifiedHaskellPackages.callPackage ./lets-lens.nix {};
in
  lets-lens
