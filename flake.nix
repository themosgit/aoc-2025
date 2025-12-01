{
  description = "AoC dev shell for ocaml!!";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ocaml
            dune_3
            ocamlPackages.base
            ocamlPackages.findlib
            ocamlPackages.ocaml-lsp
            ocamlPackages.ocamlformat
          ];

          shellHook = ''
            if command -v fish &> /dev/null; then
                exec fish
            fi
          '';
        };
      }
    );
}
