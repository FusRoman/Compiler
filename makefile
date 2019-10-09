build_asm:
	ocamlc vm/VM.ml -o vm/VM
	ocamlc assembler/Assembler.ml -o assembler/Assembler

run_asm: build_asm
	./assembler/Assembler test/$(file).asm
	./vm/VM test/$(file).btc

build_stk: build_asm
	ocamlc -c -I utils/ utils/cycle.mli utils/cycle.ml
	ocamllex stk/STKCompiler.mll
	ocamlc -I utils/ utils/cycle.cmo stk/STKCompiler.ml -o stk/STKCompiler

run_stk: build_stk
	./stk/STKCompiler test/$(file).stk
	$(MAKE) run_asm file=$(file)

build_art: build_stk
	menhir -v art/ARTParser.mly
	ocamlopt art/ART_SyntaxTree.mli
	ocamlopt -I art/ art/ART_SyntaxTree.ml
	ocamlopt -I art/ art/ARTParser.mli
	ocamlopt -I art/ art/ARTParser.ml
	ocamllex art/ARTLexer.mll
	ocamlopt -I art/ art/ARTLexer.ml
	ocamlopt -I art/ -o art/ARTCompiler art/ARTParser.cmx art/ART_SyntaxTree.cmx art/ARTLexer.cmx art/ARTCompiler.ml

run_art: build_art
	./art/ARTCompiler test/$(file).art test/$(file).stk
	$(MAKE) run_stk file=$(file)

clear:
	rm -rf stk/*.byte stk/*.cmo stk/*.cmi stk/*.ml stk/STKCompiler
	rm -rf utils/*.cmi utils/*.cmo
	rm -rf vm/*.byte vm/*.cmo vm/*.cmi vm/VM
	rm -rf assembler/*.byte assembler/*.cmo assembler/*.cmi assembler/Assembler
	rm -rf test/*.asm test/*.btc a.out
	rm -rf art/*.cmi art/*.cmx art/*.o art/*a.out art/*.conflicts art/*.automaton art/ARTLexer.ml art/ARTParser.ml art/ARTParser.mli art/ARTCompiler	
