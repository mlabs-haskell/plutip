{
  description = "plutip-core";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };

  inputs = {
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.flake = false; # Bad Nix code
    # haskell-nix.url = "github:input-output-hk/haskell.nix";
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
    # tooling.inputs.haskell-nix.follows = "haskell-nix";
    # tooling.inputs.nixpkgs.follows = "haskell-nix/nixpkgs";
    haskell-nix.follows = "tooling/haskell-nix";
    nixpkgs.follows = "tooling/nixpkgs";

    tooling.inputs.cardano-haskell-packages.follows = "CHaP";
    
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    # all inputs below this line is for pinning with haskell.nix
    cardano-addresses = {
      url =
        "github:input-output-hk/cardano-addresses";
      flake = false;
    };
    # cardano-node = {
    #   url =
    #     "github:input-output-hk/cardano-node";
    #   flake = false; # we need it to be available in shell
    # };
    cardano-wallet = {
      url = "github:input-output-hk/cardano-wallet";
      flake = false;
    };
    # hw-aeson = {
    #   url = "github:haskell-works/hw-aeson/ba7c1e41c6e54d6bf9fd1cd013489ac713fc3153";
    #   flake = false;
    # };
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    OddWord = {
      url = "github:locallycompact/OddWord";
      flake = false;
    };

  };

  outputs =
    { self, nixpkgs, haskell-nix, CHaP, iohk-nix, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays =
            [ haskell-nix.overlay (import "${iohk-nix}/overlays/crypto") ];
          inherit (haskell-nix) config;
          inherit system;
        };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      haskellModules = [
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
        ({ config, pkgs, ... }: {
          packages.plutip-core.components.tests."plutip-tests".build-tools = [
            config.hsPkgs.cardano-cli.components.exes.cardano-cli
            config.hsPkgs.cardano-node.components.exes.cardano-node
          ];
          packages.plutip-core.components.exes.local-cluster = {
            pkgconfig = [ [ pkgs.makeWrapper ] ];
            postInstall = with pkgs; ''
              wrapProgram $out/bin/local-cluster \
                --prefix PATH : "${lib.makeBinPath [
                  config.hsPkgs.cardano-cli.components.exes.cardano-cli
                  config.hsPkgs.cardano-node.components.exes.cardano-node
                ]}"
            '';
          };
        })
      ];
      extraSources = [
        {
          src = inputs.cardano-addresses;
          subdirs = [ "core" "command-line" ];
        }
        {
          src = inputs.cardano-node;
          subdirs = [
            "cardano-api"
            "cardano-cli"
            "cardano-git-rev"
            "cardano-node"
            "cardano-submit-api"
            "cardano-testnet"
            "trace-dispatcher"
            "trace-forward"
            "trace-resources"
          ];
        }
        {
          src = inputs.cardano-wallet;
          subdirs = [
            "lib/balance-tx"
            "lib/coin-selection"
            "lib/dbvar"
            "lib/launcher"
            "lib/numeric"
            "lib/primitive"
            "lib/strict-non-empty-containers"
            "lib/test-utils"
            "lib/text-class"
            "lib/wai-middleware-logging"
            "lib/wallet"
          ];
        }
        {
          src = inputs.hw-aeson;
          subdirs = [ "." ];
        }
      ];

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          project = pkgs.haskell-nix.cabalProject {
            name = "plutip-core";
            src = ./.;
            inputMap = {
              "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
            };
            inherit extraSources;
            compiler-nix-name = "ghc8107";

            shell = {
              withHoogle = true;
              exactDeps = true;

              packages = ps: [ ps.plutip-core ];

              tools.haskell-language-server = "latest";

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

                # hls doesn't support preprocessors yet so this has to exist in PATH
                haskellPackages.record-dot-preprocessor

                # Cardano tools
                project.hsPkgs.cardano-cli.components.exes.cardano-cli
                project.hsPkgs.cardano-node.components.exes.cardano-node
              ];
            };

            # inherit (bot-plutus-interface);
            modules = haskellModules;
          };
        in
        project;
    in
    # new code
    if true then
      (inputs.tooling.lib.mkFlake { inherit self; } {
        imports = [
          (inputs.tooling.lib.mkHaskellFlakeModule1 {
            project = {
              # # TODO:set? update?
              # index-state = "2022-05-25T00:00:00Z";
              src = "${self}";
              inputMap = {
                "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
              };

              modules = [({config, ...}: {
                packages.dbvar.patches = [
                  ./patch-dbvar.diff
                ];
              })];
              
              extraHackage = [
                "${inputs.OddWord}"
                "${inputs.cardano-addresses}/core"
                "${inputs.cardano-addresses}/command-line"
                "${inputs.cardano-wallet}/lib/balance-tx"
                "${inputs.cardano-wallet}/lib/coin-selection"
                "${inputs.cardano-wallet}/lib/dbvar"
                "${inputs.cardano-wallet}/lib/launcher"
                "${inputs.cardano-wallet}/lib/numeric"
                "${inputs.cardano-wallet}/lib/primitive"
                "${inputs.cardano-wallet}/lib/strict-non-empty-containers"
                "${inputs.cardano-wallet}/lib/test-utils"
                "${inputs.cardano-wallet}/lib/text-class"
                "${inputs.cardano-wallet}/lib/wai-middleware-logging"
                "${inputs.cardano-wallet}/lib/wallet"
              ];
            };
          })
        ];
      })
    else {
      inherit extraSources haskellModules;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem (system:
        let lib = "plutip:lib:plutip";
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

      # Instruction for the Hercules CI to build on x86_64-linux only, to avoid errors about systems without agents.
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
