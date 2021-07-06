{
  description = "A very basic flake";

  inputs = {
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/release-21.05";
  };

  outputs = { self, emacs-overlay, nixpkgs }:
    let
      pkgs = import nixpkgs {
        overlays = [
          emacs-overlay.overlay
          (
            final: prev: {
              myEmacs = prev.emacsWithPackagesFromUsePackage {
                alwaysEnsure = true;
                config = ./init.el;
              };
            }
          )
        ];
        system = "x86_64-linux";
      };
    in
    {
      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = with pkgs; [
          cabal-install
          cargo # NOTE: needed for nixpkgs-fmt pre-commit hook
          ghcid
          haskell-language-server
          haskellPackages.ormolu
          haskellPackages.pointfree
          hlint
          idris # TODO: idris2
          gitAndTools.pre-commit
          myEmacs
          nixpkgs-fmt
        ];
      };
    };
}
