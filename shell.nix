let sources = import ./nix/sources.nix;
    pkgs = import sources.nixpkgs {};
    github-utils = import ./default.nix;
in
github-utils.shellFor {
  withHoogle = false;
  tools = {
    cabal = "3.2.0.0";
    haskell-language-server = "0.5.1";
    hlint = "3.2.1";
    stylish-haskell = "0.12.2.0";
  };
  buildInputs = with pkgs.haskellPackages;
    [ ghcid
      hpack
    ];
  exactDeps = true;
}
