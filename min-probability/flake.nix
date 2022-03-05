{
    description = "Min probability code challenge";
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
            pkgs.openjdk11
            pkgs.maven
            pkgs.nixpkgs-fmt
          ];
      };
    };
}