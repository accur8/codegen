{
  description = "codegen project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    a8-scripts.url = "github:fizzy33/a8-scripts";
  };

  outputs = { self, nixpkgs, flake-utils, a8-scripts }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Java/Scala setup
        my-java = pkgs.openjdk11_headless;
        my-scala = pkgs.scala.override { jre = my-java; };
        my-sbt = pkgs.sbt.override { jre = my-java; };
        my-ammonite = pkgs.ammonite_2_13.override { jre = my-java; };

      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            a8-scripts.packages.${system}.a8-scripts
            my-ammonite
            my-java
            my-sbt
            my-scala
            pkgs.python3
            pkgs.gnupg
          ];
        };
      }
    );
}