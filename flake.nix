{
  description = "plutip";

  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix?ref=31b4ff7b3cf1b6537da08ca7c20f4a7ab96fe397";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    bot-plutus-interface.url =
      "github:mlabs-haskell/bot-plutus-interface?rev=07993b28810e3a5b8060297081e099d87284da6b";
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

      haskellModules = bot-plutus-interface.haskellModules ++ [
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

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          project = pkgs.haskell-nix.cabalProject {
            name = "plutip";
            src = ./.;
            sha256map = import ./nix/sha256map.nix;
            compiler-nix-name = "ghc8107";

            shell = {
              withHoogle = false;
              exactDeps = true;

              additional = ps: [ ps.bot-plutus-interface ];

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

            inherit (bot-plutus-interface);
            modules = haskellModules;
          };
        in
        project;
    in
    {
      inherit haskellModules;

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
    };
}
