{
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        lib = nixpkgs.lib;
        ghc-version = "8107";
        pkgs = nixpkgs.legacyPackages.${system};
        hkgs = if ghc-version != null then
          pkgs.haskell.packages."ghc${ghc-version}"
        else
          pkgs.haskellPackages;
      in {
        packages.neither-data = hkgs.callPackage ./neither-data.nix { };
        devShell = self.devShells.${system}.neither-data;
        devShells = let
          mkDevShell = args:
            pkgs.mkShellNoCC ({
              inputsFrom = [ self.packages.${system}.neither-data ];
            } // args);
          mkCalled = f:
            f { } // {
              __functor = self: f;
              __functionArgs = builtins.functionArgs f;
            };
        in {
          neither-data = mkDevShell { };
          withCabal = mkDevShell { packages = with pkgs; [ cabal-install ]; };
          withStack = mkDevShell { packages = with pkgs; [ stack ]; };
          withCabalHls = mkCalled ({ editor ? null }:
            mkDevShell {
              buildInputs = lib.optional (editor != null) editor
                ++ (with pkgs; [ cabal-install haskell-language-server ]);
            });
        };
      });
}
