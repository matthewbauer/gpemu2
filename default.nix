{ pkgs ? import <nixpkgs> {}
, haskellPackages ? pkgs.haskellPackages }:

haskellPackages.developPackage {
  root = ./.;

  overrides = self: super: {
    webkit2gtk3-javascriptcore = pkgs.haskell.lib.overrideCabal super.webkit2gtk3-javascriptcore { broken = false; };
  };
}
