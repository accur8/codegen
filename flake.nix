{
  description = "codegen project";

  inputs = {
    nix-pins.url = "git+ssh://git@git.accur8.net/a8/nix-pins";
  };

  outputs = { self, nixpkgs, nix-pins, devshell }:
    {
      devShells = nix-pins.lib.forEachSystem (system:
        let
          pkgs = nix-pins.pkgsFor system;
          # Add any project-specific packages or overrides here
        in {
          default = nix-pins.lib.shells.scala {
            extraInputs = [
              pkgs.python3
              pkgs.gnupg
            ];
          };
        }
      );
    };
}