{
    description = "Ghost game";
    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
        flake-utils.url = "github:numtide/flake-utils";
    };
    outputs = { self, nixpkgs, flake-utils }:
    let pkgs = nixpkgs.legacyPackages.x86_64-darwin;
    in
    {
      devShell.x86_64-darwin = pkgs.mkShell {
        buildInputs =
          [
            pkgs.ghc
            pkgs.stack
            pkgs.nixpkgs-fmt
          ];
      };
    };
}