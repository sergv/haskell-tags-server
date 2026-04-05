{
  inputs = {

    nixpkgs = {
      url = "nixpkgs";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
    };

  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem ["x86_64-linux" "i686-linux"] (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.rure
            pkgs.rure.dev
            pkgs.zlib
          ];
          LD_LIBRARY_PATH = "${pkgs.rure}/lib";
          # shellHook = ''
          #   echo "Updated rure-dev is at ${pkgs.rure.dev}"
          # '';
        };
      });
}
