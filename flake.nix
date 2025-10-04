{
  description = "codegen project";

  inputs = {
    # Using local path for testing new composable shells
    nix-pins.url = "path:/Users/glen/code/accur8/nix-pins";
    # When ready, switch back to: "git+ssh://git@git.accur8.net/a8/nix-pins";
    nixpkgs.follows = "nix-pins/nixpkgs";
  };

  outputs = { self, nix-pins, ... }:
    {
      devShells = nix-pins.lib.composeDevShells {
        default = {pkgs, frags, ...}: [
          (frags.scala_213 {
            extra = [
              pkgs.gnupg
            ];
          })
          (frags.aiCodingAssistants { })  # Already includes python312
        ];
      };
    };
}