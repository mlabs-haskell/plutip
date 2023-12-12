{
  description = "plutip-core";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    hackage-nix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackage-nix";
    };
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cardano-node.url = "github:input-output-hk/cardano-node/8.1.1";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs =
    inputs@{ self, nixpkgs, haskell-nix, CHaP, iohk-nix, cardano-node, flake-parts, ... }:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays = [ iohk-nix.overlays.crypto haskell-nix.overlay ];
          inherit (haskell-nix) config;
          inherit system;
        };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

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
        ({ config, pkgs, ... }:
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

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
        in
        pkgs.haskell-nix.cabalProject {
          name = "plutip-core";
          src = ./.;
          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
          };
          compiler-nix-name = "ghc8107";

          shell = {
            withHoogle = true;
            exactDeps = true;

            tools.haskell-language-server = "1.5.0.0"; # Newer versions failed to build

            nativeBuildInputs = with pkgs'; [
              # Haskell Tools
              haskellPackages.fourmolu
              haskellPackages.cabal-install
              haskellPackages.cabal-fmt
              nixpkgs-fmt
              hlint
              entr
              ghcid
              git
              fd

              # Cardano tools
              cardano-node.packages.${system}.cardano-cli
              cardano-node.packages.${system}.cardano-node
            ];
          };

          modules = haskellModules system;
        };

        project = perSystem projectFor;
        flake = perSystem (system: (projectFor system).flake { });
    in
      flake-parts.lib.mkFlake { inherit inputs; } {
        imports = [
          ./pre-commit.nix
        ];
        flake = {
          inherit project flake;

          # This will build all of the project's executables and the tests
          check = perSystem
            (system:
              (nixpkgsFor system).runCommand "combined-check"
                {
                  nativeBuildInputs = builtins.attrValues self.checks.${system}
                    ++ builtins.attrValues self.flake.${system}.packages;
                } ''mkdir $out''
            );

          haskellModules = perSystem haskellModules;

          # Instruction for the Hercules CI to build on x86_64-linux only, to avoid errors about systems without agents.
          herculesCI.ciSystems = [ "x86_64-linux" ];
        };
        systems = defaultSystems;
        perSystem = { system, config, pkgs, ... }: {
          packages = flake.${system}.packages 
            // {
              default = config.packages."plutip-core:lib:plutip-core";
            };

          apps = flake.${system}.apps;

          devShells = {
            # Adds pre-commit packages and shell hook to the haskell shell
            default = pkgs.mkShell {
              inputsFrom = [ config.devShells.haskell config.devShells.dev-pre-commit ];
              shellHook = config.devShells.haskell.shellHook + config.devShells.dev-pre-commit.shellHook;
            };
            haskell = flake.${system}.devShell;
          };

          checks = flake.${system}.checks;
        };
    };
}
