{
  description = "Advent of Code 2025 - Haskell solutions";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        packageName = "aoc2025";

        myDevTools = [
          haskellPackages.ghc
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.ghcid
          haskellPackages.hlint
          haskellPackages.ormolu
          haskellPackages.hoogle
          pkgs.zlib
        ];
      in
      {
        packages.${packageName} = haskellPackages.callCabal2nix packageName ./. { };

        packages.default = self.packages.${system}.${packageName};

        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;

          inputsFrom = [
            self.packages.${system}.${packageName}.env
          ];

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.${packageName}}/bin/aoc2025";
        };
      }
    );
}
