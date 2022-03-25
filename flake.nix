{
  description = "plutip";

  inputs = {
    haskell-nix.follows = "bot-plutus-interface/haskell-nix";
    nixpkgs.follows = "bot-plutus-interface/haskell-nix/nixpkgs";
    iohk-nix.follows = "bot-plutus-interface/iohk-nix";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    bot-plutus-interface.url =
      "github:mlabs-haskell/bot-plutus-interface?rev=0e3a56cad635288adecc98f41dc90d4d4a86755b";
  };

  outputs =
    { self, bot-plutus-interface, nixpkgs, haskell-nix, iohk-nix, ... }@inputs:
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

      extraSources = inputs.bot-plutus-interface.extraSources ++ [{
        src = inputs.bot-plutus-interface;
        subdirs = [ "." ];
      }];

      haskellModules = bot-plutus-interface.haskellModules ++ [
        ({ config, ... }: {
          packages.plutip.components.tests."plutip-tests".build-tools = [
            config.hsPkgs.cardano-cli.components.exes.cardano-cli
            config.hsPkgs.cardano-node.components.exes.cardano-node
          ];
        })
      ];

      projectFor = system: 
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          plutus = import inputs.plutus { inherit system; };
          src = ./.;
        in import ./nix/haskell.nix {
          inherit src inputs pkgs pkgs' system extraSources haskellModules;
          inherit (bot-plutus-interface) cabalProjectLocal;
        };

    in {
      inherit extraSources haskellModules;
      inherit (bot-plutus-interface) cabalProjectLocal;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem (system:
        let lib = "plutip:lib:plutip";
        in self.flake.${system}.packages.${lib});

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      devShell = perSystem (system: self.flake.${system}.devShell);

      # This will build all of the project's executables and the tests
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check" {
          nativeBuildInputs = builtins.attrValues self.checks.${system}
            ++ builtins.attrValues self.flake.${system}.packages
            ++ [ self.devShell.${system}.inputDerivation ];
        } "touch $out");

      # NOTE `nix flake check` will not work at the moment due to use of
      # IFD in haskell.nix
      #
      # Includes all of the packages in the `checks`, otherwise only the
      # test suite would be included
      checks = perSystem (system: self.flake.${system}.checks);
    };
}
