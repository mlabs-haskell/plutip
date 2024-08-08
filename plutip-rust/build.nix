{ inputs, ... }: {
  perSystem = { system, inputs', ... }:

    let
      rustFlake = inputs.flake-lang.lib."${system}".rustFlake
        {
          src = ./.;
          crateName = "plutip-rust";
          cargoNextestExtraArgs = "--no-capture";

          extraSources = [
            inputs'.tx-village.packages.tx-bakery-rust-src
          ];
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
