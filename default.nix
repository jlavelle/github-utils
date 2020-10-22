let
  sources = import ./nix/sources.nix;
  compilerVersion = "ghc884";
  hasknix = import sources.hasknix {};
  pkgs = (import hasknix.sources.nixpkgs-2003) hasknix.nixpkgsArgs;
in
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "github-utils";
    src = ./.;
  };
  compiler-nix-name = compilerVersion;
}
