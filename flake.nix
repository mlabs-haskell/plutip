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

  };

  outputs =
    { self, nixpkgs, haskell-nix, CHaP, iohk-nix, cardano-node, ... }:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays = [
            iohk-nix.overlays.crypto
            iohk-nix.overlays.haskell-nix-crypto
            haskell-nix.overlay
          ];
          inherit (haskell-nix) config;
          inherit system;
        };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      haskellModules = system: [
        ({ pkgs, ... }:
          {
            packages = {
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
    in
    {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem (system:
        let lib = "plutip-core:lib:plutip-core";
        in self.flake.${system}.packages.${lib});

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      devShell = perSystem (system: self.flake.${system}.devShell);

      # This will build all of the project's executables and the tests
      check = perSystem
        (system:
          (nixpkgsFor system).runCommand "combined-check"
            {
              nativeBuildInputs = builtins.attrValues self.checks.${system}
                ++ builtins.attrValues self.flake.${system}.packages;
            } ''mkdir $out''
        );

      checks = perSystem (system:
        self.flake.${system}.checks // {
          formatting = (nixpkgsFor system).runCommand "formatting-check"
            {
              nativeBuildInputs = [
                self.devShell.${system}.inputDerivation
                self.devShell.${system}.nativeBuildInputs
              ];
            }
            ''
              cd ${self}
              export LC_CTYPE=C.UTF-8
              export LC_ALL=C.UTF-8
              export LANG=C.UTF-8
              export IN_NIX_SHELL='pure'
              # this check is temporarily skipped in CI due to a bug in
              # fourmolu:
              #
              # ```
              # Formatting is not idempotent:
              #   src/Plutip/Launch/Cluster.hs<rendered>:753:19
              #   before: "       sgs\n         "
              #   after:  "       sgs{ Ledger.s"
              # Please, consider reporting the bug.
              # ```
              # make format_check cabalfmt_check nixpkgsfmt_check lint
              mkdir $out
            '';
        });

      haskellModules = perSystem haskellModules;

      # Instruction for the Hercules CI to build on x86_64-linux only, to avoid errors about systems without agents.
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
