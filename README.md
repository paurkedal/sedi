## Synopsis

`sedi` is a refactoring tool based on `sed` and `diff`.  It works like `sed
-i` but interactively asks to confirm the changes to be made before
modifying the original files.

## Installation

Install `opam` from your distribution or from
[opam.ocaml.org](http://opam.ocaml.org/), then
```shell
opam init
# set your environment as described
opam pin add sedi https://github.com/paurkedal/sedi.git
```
`sedi` should now be in the `$PATH` provided by the instructions printed out
by `opam init`.
