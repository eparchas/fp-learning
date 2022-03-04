{
    description = "Min probability code challenge";
    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };
    outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
    {
      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs =
          [
            pkgs.openjdk11
            pkgs.maven
            pkgs.nixpkgs-fmt
          ];
      };
    };
}