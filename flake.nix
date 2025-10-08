{
  description = "codegen project";

  inputs = {
    nix-pins.url = "git+ssh://git@git.accur8.net/a8/nix-pins";
  };

  outputs =
    { self, nix-pins, ... }:
    {
      devShells = nix-pins.lib.composeDevShells {
        default =
          { pkgs, frags, ... }:
          [
            (frags.scala_213 {
              extra = [
                pkgs.gnupg
              ];
            })
            (frags.aiCodingAssistants { }) # Already includes python312
          ];
      };
    };
}
