let 
  src = import ./nix/sources.nix;
in 
{ pkgs ? import src.nixpkgs {} 
, ... 
}:
let
  extraDeps =
    if pkgs.lib.trivial.inNixShell
      then with pkgs.haskellPackages; [ 
          cabal-install
          ghcid 
          # haskell-language-server  -- Let VSCode download latest
        ]
      else [];
in 
  pkgs.haskellPackages.developPackage {
    root = ./.;
    name = "zulip-archive";
    source-overrides = {
      rib = src.rib + /rib;
      rib-core = src.rib + /rib-core;
    };
    overrides = self: super: with pkgs.haskell.lib; {
    };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv extraDeps;
  }