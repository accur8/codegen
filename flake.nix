{
  description = "codegen project";

  inputs = {
    nix-pins.url = "git+ssh://git@git.accur8.net/a8/nix-pins";
    nixpkgs.follows = "nix-pins/nixpkgs";
    devshell.follows = "nix-pins/devshell";
  };

  outputs = { self, nixpkgs, nix-pins, devshell }:
    {
      devShells = nix-pins.lib.forEachSystem (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              nix-pins.overlays.default
              devshell.overlays.default
            ];
          };

          # Java/Scala setup with JDK 11
          my-java = pkgs.openjdk11_headless;
        in {
          default = nix-pins.lib.shells.scala {
            inherit pkgs;
            jdk = my-java;
            extraInputs = [
              pkgs.python3
              pkgs.gnupg
            ];
          };
        });
    };
}