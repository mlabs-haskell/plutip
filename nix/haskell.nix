{ src, inputs, pkgs, pkgs', system, extraSources, cabalProjectLocal, haskellModules }:

pkgs.haskell-nix.cabalProject {
  inherit src;

  name = "plutip";

  compiler-nix-name = "ghc8107";

  shell = {
    additional = ps: [ ps.bot-plutus-interface ];

    withHoogle = true;

    tools.haskell-language-server = "latest";

    exactDeps = true;

    nativeBuildInputs = with pkgs';
      [
        # Haskell Tools
        haskellPackages.fourmolu
        haskellPackages.cabal-install
        hlint
        entr
        ghcid
        git
        nixpkgs-fmt

        # hls doesn't support preprocessors yet so this has to exist in PATH
        haskellPackages.record-dot-preprocessor

        # Cardano tools
        # FIXME: use project.hsPkgs.cardano-node.components.executables.<something>
        # inputs.cardano-node.packages.${system}.cardano-node
        # inputs.cardano-node.packages.${system}.cardano-cli
      ];
  };

  inherit cabalProjectLocal extraSources;

  modules = haskellModules system;
}
