{
  outputs = { self, nixpkgs, flake-utils }:
    let
      preCall = with builtins;
        f:
        let
          g = a:
            if isAttrs a then
              let x = f a;
              in {
                __functor = self: b: g b;
              } // x // {
                ${if x ? __functor then null else "__functionArgs"} =
                  functionArgs f;
              }
            else
              f a;
        in g { };
    in preCall (fArgs@{ ghcVersion ? "8107" }:
      flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
        let
          self' = self fArgs;
          lib = nixpkgs.lib;
          pkgs = nixpkgs.legacyPackages.${system};
          hkgs = if ghcVersion != null then
            pkgs.haskell.packages."ghc${ghcVersion}"
          else
            pkgs.haskellPackages;
        in {
          defaultPackage = self'.packages.default;
          # defaultPackage is deprecated as of Nix 2.7.0 in favour of packages.<system>.default
          packages = rec {
            neither-data = hkgs.callPackage ./neither-data.nix { };
            default = neither-data;
          };
          devShell = self'.devShells.${system}.default;
          # devShell is deprecated as of Nix 2.7.0 in favour of devShells.<system>.default
          devShells = let
            mkDevShell = args:
              pkgs.mkShellNoCC
              ({ inputsFrom = [ self'.packages.${system}.default ]; } // args);
          in rec {
            default = neither-data;
            neither-data = mkDevShell { };
            withCabal = mkDevShell { packages = with pkgs; [ cabal-install ]; };
            withStack = mkDevShell { packages = with pkgs; [ stack ]; };
            withCabalHls = preCall ({ editor ? null }:
              mkDevShell {
                buildInputs = lib.optional (editor != null) editor
                  ++ (with pkgs; [ cabal-install haskell-language-server ]);
              });
          };
        }));
}
