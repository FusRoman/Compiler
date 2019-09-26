#!/bin/sh

echo "build.sh [<file>]"
echo "Compiles STKCompiler.mll and <file>, then run it"
echo "<file> : name of a STK file without extension"
echo "If not given, test/prog1.stk will be compiled and run instead."
echo ""

if [ $# -eq 0 ]
then
  set "test/prog1"
fi 

echo "Compiling stk/STKCompiler.mll..."
ocamllex stk/STKCompiler.mll
if [ $? -eq 0 ]
then
  echo "\nCompiling stk/ocaml/STKCompiler.ml..."
else
  echo "Compilation of stk/STKCompiler.mll failed."
  exit 1
fi

cd stk
mkdir ocaml
mv STKCompiler.ml ocaml/STKCompiler.ml
cd ocaml
ocamlbuild STKCompiler.ml STKCompiler.byte
if [ $? -eq 0 ]
then
  echo "\nCompiling ${1}.stk..."
else
  echo "Compilation of stk/ocaml/STKCompiler.ml failed."
  exit 1
fi

mv _build/STKCompiler.byte ../STKCompiler.byte
cd ../..
./stk/STKCompiler.byte $1.stk
if [ $? -eq 0 ]
then
  echo "\nCompiling ${1}.asm..."
else
  echo "Compilation of ${1}.stk failed."
  exit 1
fi

./assembler/Assembler.byte $1.asm
if [ $? -eq 0 ]
then
  echo "Done\n\nExecuting ${1}.btc..."
else
  echo "Compilation of ${1}.asm failed."
  exit 1
fi

./vm/VM.byte $1.btc
exit $?