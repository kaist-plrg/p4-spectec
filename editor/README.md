# Editor support

The `vim/` and `emacs/` directories provide syntax highlighting and language-server registration for `.watsup` files.

## Language server

`p4spectec-lsp` publishes parse and elaboration diagnostics when a file is opened or edited. Create the `5.1.0` switch using the [installation instructions](../README.md#ocaml-compiler-and-packages) if it does not exist. From the repository root, install the declared dependencies into that switch, build the server, and put it on your `PATH`:

```shell
opam install --switch=5.1.0 dune
opam exec --switch=5.1.0 -- dune build p4spectec.opam
opam install --switch=5.1.0 . --deps-only
make lsp
mkdir -p ~/.local/bin
ln -sf "$PWD/p4spectec-lsp" ~/.local/bin/p4spectec-lsp
```

The dependencies include `linol-eio` 0.10 or later and `eio_main`. Rerun the setup commands when updating an existing checkout so the switch includes the current project dependencies.

The Makefile uses the `5.1.0` opam switch. `opam install --switch=5.1.0 .` also installs the server executable into that switch.

The nearest ancestor containing a `*.spec` file is the specification root. Marker files can be empty. The server visits directory entries in lexicographic order and gathers `.watsup` files recursively, excluding `include` directories. New unsaved files follow the same order. The server checks the collected files together. The P4 spec has a marker in `spec/`. The AL and SL meta-specs have separate markers in `spec-meta/al/` and `spec-meta/sl/`.

The edited file uses its unsaved buffer contents. Sibling files use their contents on disk. A file outside a marked root is checked alone. Elaboration stops at the first error and retains warnings emitted before that error. A sibling error also appears at the start of the edited file, with a link to its original location. Sibling warnings remain local to their own file. Diagnostics are cleared when an edit succeeds or the document closes.

AL and SL keep identical copies of their common definitions under their respective `0-common/` directories. Each copy is checked with its own meta-spec. Apply changes to these definitions in both directories.

## Neovim 0.11 or later

Add `editor/vim/` to your runtime path and enable the server in your configuration:

```lua
vim.opt.runtimepath:prepend("/path/to/p4-spectec/editor/vim")
vim.lsp.enable("p4spectec")
```

Open a `.watsup` file to start diagnostics. The configuration in `vim/lsp/p4spectec.lua` runs `p4spectec-lsp` from `PATH`. Classic Vim uses the syntax highlighter without the language server.

## Emacs with Eglot

Load the major mode:

```elisp
(add-to-list 'load-path "/path/to/p4-spectec/editor/emacs")
(require 'watsup-mode)
```

Open a `.watsup` file and run `M-x eglot`. The mode registers `p4spectec-lsp` with Eglot. Emacs 29 and later include Eglot. On earlier Emacs versions, install Eglot separately.

## Tests

`make test-lsp` runs diagnostic snapshots and drives the server over standard input and output, including incremental edits, Unicode ranges, clearing diagnostics, and shutdown.
