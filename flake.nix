{
  description = "plutip-core";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs";

    cardano-node.url = "github:input-output-hk/cardano-node/8.1.1";

    flake-parts.url = "github:hercules-ci/flake-parts";
    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";
    hci-effects.url = "github:hercules-ci/hercules-ci-effects";
  };

  outputs =
    inputs@{ nixpkgs, haskell-nix, CHaP, iohk-nix, cardano-node, flake-parts, ... }:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      haskellModules = system: [
        ({ pkgs, ... }:
          {
            packages = {
              cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
              cardano-wallet.components.library.build-tools = [
                pkgs.buildPackages.buildPackages.gitMinimal
              ];
              cardano-config.components.library.build-tools = [
                pkgs.buildPackages.buildPackages.gitMinimal
              ];
            };
          }
        )
        ({ pkgs, ... }:
          let
            nodeExes = cardano-node.packages.${system};
          in
          {
            packages.plutip-core.components.tests.plutip-tests = {
              pkgconfig = [ [ pkgs.makeWrapper ] ];
              postInstall = with pkgs; ''
                wrapProgram $out/bin/plutip-tests \
                  --prefix PATH : "${lib.makeBinPath [
                    nodeExes.cardano-node
                    nodeExes.cardano-cli
                  ]}"
              '';
            };
            packages.plutip-core.components.exes.local-cluster = {
              pkgconfig = [ [ pkgs.makeWrapper ] ];
              postInstall = with pkgs; ''
                wrapProgram $out/bin/local-cluster \
                  --prefix PATH : "${lib.makeBinPath [
                    nodeExes.cardano-node
                    nodeExes.cardano-cli
                  ]}"
              '';
            };
          })
      ];
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./pre-commit.nix
        ./hercules-ci.nix
      ];
      flake = {
        haskellModules = perSystem haskellModules;
      };
      systems = defaultSystems;
      perSystem = { system, config, pkgs, pkgsForHaskellNix, ... }:
        let
          project =
            pkgsForHaskellNix.haskell-nix.cabalProject {
              name = "plutip-core";
              src = ./.;
              inputMap = {
                "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
              };
              compiler-nix-name = "ghc8107";

              shell = {
                withHoogle = true;
                exactDeps = true;

                tools = {
                  # haskell-language-server = { }; # FIXME(bladyjoker): Can't use this really until we upgrade GHC
                  cabal = { };
                  ghcid = { };
                };

                nativeBuildInputs = with pkgsForHaskellNix; [
                  entr
                  git
                  fd

                  # Cardano tools
                  cardano-node.packages.${system}.cardano-cli
                  cardano-node.packages.${system}.cardano-node
                ];
              };

              modules = haskellModules system;
            };

          flake = project.flake { };

        in
        {
          _module.args = {
            pkgs = import nixpkgs {
              inherit system;
            };

            pkgsForHaskellNix = import inputs.haskell-nix.inputs.nixpkgs {
              overlays = [ iohk-nix.overlays.crypto haskell-nix.overlay ];
              inherit (haskell-nix) config;
              inherit system;
            };
          };

          packages = flake.packages
            // {
            default = config.packages."plutip-core:lib:plutip-core";
          };

          inherit (flake) apps checks;

          devShells = {
            # Adds pre-commit packages and shell hook to the haskell shell
            default = pkgs.mkShell {

              inputsFrom = [ config.devShells.haskell config.devShells.dev-pre-commit ];
              shellHook = config.devShells.haskell.shellHook + config.devShells.dev-pre-commit.shellHook;
            };
            haskell = flake.devShell;
          };
        };
    };
}
