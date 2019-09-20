#!/bin/sh

echo 'Compiling ocamllex/STKCompiler.mll...'
ocamllex ocamllex/STKCompiler.mll
echo 'Done\nCompiling ocaml/STKCompiler.ml...'
mv ocamllex/STKCompiler.ml ocaml/STKCompiler.ml
cd ocaml
ocamlbuild STKCompiler.ml STKCompiler.byte
echo 'Done'
