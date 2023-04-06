{
  description = "plutip-core";

  inputs = {
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
    haskell-nix.follows = "tooling/haskell-nix";
    nixpkgs.follows = "tooling/nixpkgs";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.flake = false; # Bad Nix code

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    # all inputs below this line is for pinning with haskell.nix
    cardano-addresses = {
      url =
        "github:input-output-hk/cardano-addresses/5094fb9d304ed69adedc99513634a00cbf850fca";
      flake = false;
    };
    cardano-node = {
      url =
        "github:input-output-hk/cardano-node/ebc7be471b30e5931b35f9bbc236d21c375b91bb";
      flake = false; # we need it to be available in shell
    };
    cardano-wallet = {
      url = "github:input-output-hk/cardano-wallet/bbf11d4feefd5b770fb36717ec5c4c5c112aca87";
      flake = false;
    };
    cardano-wallet' = {
      url = "github:input-output-hk/cardano-wallet";
      flake = false;
    };
    ouroboros-network = {
      url = "github:input-output-hk/ouroboros-network";
      flake = false;
    };
    hw-aeson = {
      url = "github:haskell-works/hw-aeson/ba7c1e41c6e54d6bf9fd1cd013489ac713fc3153";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    OddWord = {
      url = "github:locallycompact/OddWord";
      flake = false;
    };
    typed-protocols = {
      url = "github:input-output-hk/typed-protocols";
      flake = false;
    };
    io-sim = {
      url = "github:input-output-hk/io-sim";
      flake = false;
    };
    protolude = {
      url = "github:protolude/protolude?rev=e40b7351ec88093169f628fcddc0e3f46c74815f"; # tag/0.3.2;
      flake = false;
    };
  };

  outputs =
    { self, nixpkgs, haskell-nix, CHaP, iohk-nix, ... }@inputs:
    let
      haskellModules = [
        ({ config, ... }: {
          packages.ouroboros-consensus.patches = [
            ./patch-ouroboros-consensus.diff
          ];
          packages.cardano-api.patches = [
            ./patch-cardano-api.diff
          ];
          packages.cardano-ledger-byron.patches = [
            ./patch-cardano-ledger-byron.diff
          ];
          packages.ouroboros-consensus-byron.patches = [
            ./patch-ouroboros-consensus-byron.diff
          ];
          packages.ouroboros-consensus-shelley.patches = [
            ./patch-ouroboros-consensus-shelley.diff
          ];
          packages.ouroboros-network.patches = [
            ./patch-ouroboros-network.diff
          ];
        })
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
      extraHackage' = [
        "${inputs.OddWord}"
      ];
    in
    inputs.tooling.lib.mkFlake { inherit self; } ({ lib, ... }:
    let
      mkExtraHackage = srcs: lib.concatMap ({ src, subdirs }: builtins.map (subdir: "${src}/${subdir}") subdirs) srcs;
    in
    {
      systems = [ "x86_64-linux" "x86_64-darwin" ];
      imports = [
        (inputs.tooling.lib.mkHaskellFlakeModule1 {
          project = ({ pkgs, ... }: {
            # # TODO:set? update?
            # index-state = "2022-05-25T00:00:00Z";
            src = "${self}";
            inputMap = {
              "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
            };
            compiler-nix-name = lib.mkForce "ghc8107";
            modules = haskellModules;
            extraHackage = extraHackage' ++ (mkExtraHackage extraSources);
            cabalProjectLocal = lib.mkForce "";
            shell = {
              withHoogle = true;
              exactDeps = true;

              packages = ps: [ ps.plutip-core ];

              nativeBuildInputs = with pkgs; [
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
                # FIXME: is now impossible to backreference to project within mkHaskellFlakeModule1
                # for workaround -- I added cardano-node as direct depenency via .cabal
                # Issue in tooling -- https://github.com/mlabs-haskell/mlabs-tooling.nix/issues/42
                #project.hsPkgs.cardano-cli.components.exes.cardano-cli
                #project.hsPkgs.cardano-node.components.exes.cardano-node
              ];
            };
          });
        })
      ];
      perSystem = { self', pkgs, ... }: {
        checks = { };
      };
    });
}
