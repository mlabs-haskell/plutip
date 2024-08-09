{ inputs, ... }: {
  imports = [
    inputs.hci-effects.flakeModule # Adds hercules-ci and herculesCI options
  ];

  hercules-ci.flake-update = {
    enable = true;
    updateBranch = "updated-flake-lock";
    # Next two parameters should always be set explicitly
    createPullRequest = true;
    autoMergeMethod = null;
    when = {
      # Perform update by Sundays at 12:45
      minute = 45;
      hour = 12;
      dayOfWeek = "Sun";
    };
  };

  hercules-ci.github-pages.branch = "master";

  perSystem = { config, pkgs, ... }: {
    hercules-ci.github-pages.settings.contents = pkgs.stdenv.mkDerivation {
      name = "plutip-rust-github-pages";
      src = config.packages.plutip-rust-rust-doc;
      buildPhase = ''
        mkdir $out
        cp -r -L -v $src/share/doc/* $out/
        echo '<meta http-equiv="refresh" content="0; url=plutip_rust">' > $out/index.html
      '';
    };
  };

  herculesCI.ciSystems = [ "x86_64-linux" "x86_64-darwin" ];
}
