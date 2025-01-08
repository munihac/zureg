{
  description = "zureg";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs :
    inputs.flake-utils.lib.eachDefaultSystem (system:
     let pkgs = inputs.nixpkgs.legacyPackages.${system};
         haskell = pkgs.haskell.packages.ghc98;
     in
  {
    packages = {
      default = haskell.callCabal2nix "zureg" ./. {};
    };
    devShells = {
      default = pkgs.mkShell {
        packages = [
          pkgs.cabal-install
          pkgs.entr
          pkgs.postgresql
          haskell.stylish-haskell
          (haskell.ghc.withPackages (p:
            inputs.self.packages.${system}.default.buildInputs ++
            [p.postgresql-simple]))
        ];
      };
    };
  });
}
