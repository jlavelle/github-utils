let sources = import ./nix/sources.nix;
    pkgs = import sources.nixpkgs {};
    github-utils = import ./default.nix;
in
github-utils.shellFor {
  withHoogle = true;
  tools = {
    cabal = "3.2.0.0";
    haskell-language-server = "0.5.1";
    hlint = "3.2.1";
  };
  buildInputs = with pkgs.haskellPackages;
    [ ghcid
      hpack
      stylish-haskell
      brittany
    ];
  exactDeps = true;
}
