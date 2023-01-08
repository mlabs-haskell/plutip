{
  description = "plutip";

  inputs = {
    haskell-nix.url = "github:mlabs-haskell/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.flake = false; # Bad Nix code

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
        # all inputs below here are for pinning with haskell.nix
    cardano-addresses = {
      url =
        "github:input-output-hk/cardano-addresses/5094fb9d304ed69adedc99513634a00cbf850fca";
      flake = false;
    };
    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/0f3a867493059e650cda69e20a5cbf1ace289a57";
      flake = false;
    };
    cardano-config = {
      url =
        "github:input-output-hk/cardano-config/1646e9167fab36c0bff82317743b96efa2d3adaa";
      flake = false;
    };
    cardano-crypto = {
      url =
        "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
      flake = false;
    };
    cardano-ledger = {
      url =
        "github:input-output-hk/cardano-ledger/c7c63dabdb215ebdaed8b63274965966f2bf408f";
      flake = false;
    };
    cardano-node = {
      url =
        "github:input-output-hk/cardano-node/ebc7be471b30e5931b35f9bbc236d21c375b91bb";
      flake = false; # we need it to be available in shell
    };
    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
      flake = false;
    };
    cardano-wallet = {
      url = "github:input-output-hk/cardano-wallet/bbf11d4feefd5b770fb36717ec5c4c5c112aca87";
      flake = false;
    };
    ekg-forward = {
      url = "github:input-output-hk/ekg-forward/297cd9db5074339a2fb2e5ae7d0780debb670c63";
      flake = false;
    };
    ekg-json = {
      url = "github:vshabanov/ekg-json/00ebe7211c981686e65730b7144fbf5350462608";
      flake = false;
    };
    # We don't actually need this. Removing this might make caching worse?
    flat = {
      url =
        "github:Quid2/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
    goblins = {
      url =
        "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };
    hedgehog-extras = {
      url = "github:input-output-hk/hedgehog-extras/714ee03a5a786a05fc57ac5d2f1c2edce4660d85";
      flake = false;
    };
    hw-aeson = {
      url = "github:haskell-works/hw-aeson/ba7c1e41c6e54d6bf9fd1cd013489ac713fc3153";
      flake = false;
    };
    iohk-monitoring-framework = {
      url =
        "github:input-output-hk/iohk-monitoring-framework/066f7002aac5a0efc20e49643fea45454f226caa";
      flake = false;
    };
    io-sim = {
      url =
        "github:input-output-hk/io-sim/57e888b1894829056cb00b7b5785fdf6a74c3271";
      flake = false;
    };
    optparse-applicative = {
      url =
        "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };
    ouroboros-network = {
      url =
        "github:input-output-hk/ouroboros-network/cb9eba406ceb2df338d8384b35c8addfe2067201";
      flake = false;
    };
    plutus-core = {
      url =
        "github:input-output-hk/plutus/a56c96598b4b25c9e28215214d25189331087244";
      flake = false;
    };
    plutus-apps = {
      url =
        "github:input-output-hk/plutus-apps/a2045141fc0b4f14470ebf4679c6abe40aac4db7";
      flake = false;
    };
    purescript-bridge = {
      url =
        "github:input-output-hk/purescript-bridge/47a1f11825a0f9445e0f98792f79172efef66c00";
      flake = false;
    };
    quickcheck-dynamic = {
      url = "github:input-output-hk/quickcheck-dynamic/c272906361471d684440f76c297e29ab760f6a1e";
      flake = false;
    };
    servant-purescript = {
      url =
        "github:input-output-hk/servant-purescript/44e7cacf109f84984cd99cd3faf185d161826963";
      flake = false;
    };
    typed-protocols = {
      url =
        "github:input-output-hk/typed-protocols/181601bc3d9e9d21a671ce01e0b481348b3ca104";
      flake = false;
    };
    Win32-network = {
      url =
        "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
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
          packages.plutip.components.tests."plutip-tests".build-tools = [
            config.hsPkgs.cardano-cli.components.exes.cardano-cli
            config.hsPkgs.cardano-node.components.exes.cardano-node
          ];
          packages.plutip.components.exes.plutip-server = {
            pkgconfig = [ [ pkgs.makeWrapper ] ];
            postInstall = with pkgs; ''
              wrapProgram $out/bin/plutip-server \
                --prefix PATH : "${lib.makeBinPath [
                  config.hsPkgs.cardano-cli.components.exes.cardano-cli
                  config.hsPkgs.cardano-node.components.exes.cardano-node
                ]}"
            '';
          };
          packages.plutip.components.exes.local-cluster = {
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
#        {
#          src = inputs.cardano-base;
#          subdirs = [
#            "base-deriving-via"
#            "binary"
#            "binary/test"
#            "cardano-crypto-class"
#            "cardano-crypto-praos"
#            "cardano-crypto-tests"
#            "measures"
#            "orphans-deriving-via"
#            "slotting"
#            "strict-containers"
#          ];
#        }
#        {
#          src = inputs.cardano-crypto;
#          subdirs = [ "." ];
#        }
#        {
#          src = inputs.cardano-ledger;
#          subdirs = [
#            "eras/alonzo/impl"
#            "eras/alonzo/test-suite"
#            "eras/babbage/impl"
#            "eras/byron/chain/executable-spec"
#            "eras/byron/crypto"
#            "eras/byron/crypto/test"
#            "eras/byron/ledger/executable-spec"
#            "eras/byron/ledger/impl"
#            "eras/byron/ledger/impl/test"
#            "eras/shelley/impl"
#            "eras/shelley/test-suite"
#            "eras/shelley-ma/impl"
#            "eras/shelley-ma/test-suite"
#            "libs/cardano-data"
#            "libs/cardano-ledger-core"
#            "libs/cardano-ledger-pretty"
#            "libs/cardano-protocol-tpraos"
#            "libs/vector-map"
#            "libs/non-integral"
#            "libs/set-algebra"
#            "libs/small-steps"
#            "libs/small-steps-test"
#          ];
#        }
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
#        {
#          src = inputs.cardano-config;
#          subdirs = [ "." ];
#        }
#        {
#          src = inputs.cardano-prelude;
#          subdirs = [ "cardano-prelude" "cardano-prelude-test" ];
#        }
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
#        {
#          src = inputs.ekg-forward;
#          subdirs = [ "." ];
#        }
#        {
#          src = inputs.ekg-json;
#          subdirs = [ "." ];
#        }
#        {
#          src = inputs.flat;
#          subdirs = [ "." ];
#        }
#        {
#          src = inputs.goblins;
#          subdirs = [ "." ];
#        }
#        {
#          src = inputs.hedgehog-extras;
#          subdirs = [ "." ];
#        }
        {
          src = inputs.hw-aeson;
          subdirs = [ "." ];
        }
#        {
#          src = inputs.iohk-monitoring-framework;
#          subdirs = [
#            "contra-tracer"
#            "iohk-monitoring"
#            "tracer-transformers"
#            "plugins/backend-ekg"
#            "plugins/backend-aggregation"
#            "plugins/backend-monitoring"
#            "plugins/backend-trace-forwarder"
#          ];
#        }
#        {
#          src = inputs.io-sim;
#          subdirs = [
#            "io-classes"
#            "io-sim"
#            "strict-stm"
#          ];
#        }
#        {
#          src = inputs.optparse-applicative;
#          subdirs = [ "." ];
#        }
#        {
#          src = inputs.ouroboros-network;
#          subdirs = [
#            "monoidal-synchronisation"
#            "network-mux"
#            "ntp-client"
#            "ouroboros-consensus"
#            "ouroboros-consensus-byron"
#            "ouroboros-consensus-cardano"
#            "ouroboros-consensus-protocol"
#            "ouroboros-consensus-shelley"
#            "ouroboros-network"
#            "ouroboros-network-framework"
#            "ouroboros-network-testing"
#          ];
#        }
#        {
#          src = inputs.plutus-core;
#          subdirs = [
#            "plutus-core"
#            "plutus-ledger-api"
#            "plutus-tx"
#            "plutus-tx-plugin"
#            "prettyprinter-configurable"
#            "stubs/plutus-ghc-stub"
#            "word-array"
#          ];
#        }
#        {
#          src = inputs.plutus-apps;
#          subdirs = [
#            "cardano-streaming"
#            "doc"
#            "freer-extras"
#            "marconi"
#            "marconi-mamba"
#            "playground-common"
#            "pab-blockfrost"
#            "plutus-chain-index"
#            "plutus-chain-index-core"
#            "plutus-contract"
#            "plutus-contract-certification"
#            "plutus-example"
#            "plutus-ledger"
#            "plutus-ledger-constraints"
#            "plutus-pab"
#            "plutus-pab-executables"
#            "plutus-playground-server"
#            "plutus-script-utils"
#            "plutus-tx-constraints"
#            "plutus-use-cases"
#            "rewindable-index"
#            "web-ghc"
#          ];
#        }
#        {
#          src = inputs.purescript-bridge;
#          subdirs = [ "." ];
#        }
#        {
#          src = inputs.quickcheck-dynamic;
#          subdirs = [ "." ];
#        }
#        {
#          src = inputs.servant-purescript;
#          subdirs = [ "." ];
#        }
#        {
#          src = inputs.typed-protocols;
#          subdirs = [
#            "typed-protocols"
#            "typed-protocols-cborg"
#            "typed-protocols-examples"
#          ];
#        }
#        {
#          src = inputs.Win32-network;
#          subdirs = [ "." ];
#        }
      ];

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          project = pkgs.haskell-nix.cabalProject {
            name = "plutip";
            src = ./.;
            inputMap = {
              "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
            };
            inherit extraSources;
            compiler-nix-name = "ghc8107";

            shell = {
              withHoogle = true;
              exactDeps = true;

              packages = ps: [ ps.plutip ];

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
    {
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
              make format_check cabalfmt_check nixpkgsfmt_check lint
              mkdir $out
            '';
        });

      # Instruction for the Hercules CI to build on x86_64-linux only, to avoid errors about systems without agents.
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
