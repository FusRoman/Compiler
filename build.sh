#!/bin/sh

echo 'Compiling ocamllex/STKCompiler.mll...'
ocamllex ocamllex/STKCompiler.mll
echo 'Done\nCompiling ocaml/STKCompiler.ml...'
mv ocamllex/STKCompiler.ml ocaml/STKCompiler.ml
cd ocaml
ocamlbuild STKCompiler.ml STKCompiler.byte
echo 'Done\nCompiling test/prog1.stk...'
mv _build/STKCompiler.byte ../STKCompiler.byte
cd ..
./STKCompiler.byte test/prog1.stk prog1.asm
echo 'Done\nCompiling prog1.asm...'
./assembler/Assembler.byte prog1.asm
echo 'Done\nExecuting prog1.btc...'
./vm/VM.byte prog1.btc
echo 'Done!'
