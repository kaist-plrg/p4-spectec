{
  description = "P4-SpecTec";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # Use the OCaml 5.1 package set
        ocamlPkgs = pkgs.ocaml-ng.ocamlPackages_5_1;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            # The Compiler
            ocamlPkgs.ocaml

            # Project dependencies
            ocamlPkgs.dune_3
            ocamlPkgs.bignum
            ocamlPkgs.menhir
            ocamlPkgs.menhirLib
            ocamlPkgs.core
            ocamlPkgs.core_unix
            ocamlPkgs.bisect_ppx
            ocamlPkgs.yojson
            ocamlPkgs.ppx_deriving_yojson
            ocamlPkgs.ocamlformat

            # Tooling
            ocamlPkgs.ocaml-lsp
            ocamlPkgs.utop
            pkgs.opam
          ];

          shellHook = ''
            export OPAMROOT=$PWD/.opam
            if [ ! -d "$OPAMROOT" ]; then
              opam init --bare --disable-sandboxing -y
              opam switch create 5.1.0 ocaml-system --no-install -y
            fi
            eval $(opam env)
            echo "P4-SpecTec Environment Loaded"
          '';
        };
      }
    );
}
