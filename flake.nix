{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix?rev=5f4b3e24c25ee28dc766803e3e83912a8b5c3926";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, fenix }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell { buildInputs = with pkgs; [ python3 fenix.packages.${system}.latest.toolchain ]; };
      });
}
