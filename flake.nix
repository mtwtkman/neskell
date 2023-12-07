let
  ps1 = builtins.getEnv "PS1";
in
{
  description = "NES emulator implemented by Haskell";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig.bash-prompt = "[dev]${ps1}";
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = import ./nix/shell.nix { inherit pkgs; };
      }
    );

}
