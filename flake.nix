{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "flake-utils";

    lilypond-2-24 = {
      url = "github:lilypond/lilypond/v2.24.4";
      flake = false;
    };
    lilypond-2-25 = {
      url = "github:lilypond/lilypond/v2.25.18";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    lilypond-2-24,
    lilypond-2-25,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages.lilypond-2-24 = pkgs.lilypond.overrideAttrs {
          version = "2.24.4";
          src = lilypond-2-24;
        };
        packages.lilypond-2-25 = pkgs.lilypond.overrideAttrs {
          version = "2.25.18";
          src = lilypond-2-25;
        };
      }
    );
}
