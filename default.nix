{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) stdenv haskellPackages;
  ghc = (haskellPackages.ghcWithPackages (p:
    [ p.parsec p.hoogle haskellPackages.HDBC
      haskellPackages.HDBC-sqlite3
      haskellPackages.singletons
    ] ));
in stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    ghc
  ];

  shellHook = ''
    # hoogle generate
    # hoogle server --local -p 8080 &> /tmp/hoogle.log & disown
  '';
}
