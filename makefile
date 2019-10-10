build_asm:
	ocamlc -c -I utils/ utils/arith.mli utils/arith.ml
	ocamlc -I utils/ utils/arith.cmo vm/VM.ml -o vm/VM
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
	ocamlc -c -I utils/ utils/tagset.mli utils/tagset.ml
	ocamlc -c -I utils/ utils/ARTTree.mli utils/ARTTree.ml
	ocamlc -c -I art/ -I utils/ art/ARTParser.mli art/ARTParser.ml
	ocamllex art/ARTLexer.mll
	ocamlc -c -I art/ art/ARTLexer.ml
	ocamlc -I utils/ -I art/ utils/tagset.cmo utils/ARTTree.cmo art/ARTLexer.cmo art/ARTParser.cmo art/ARTCompiler.ml -o art/ARTCompiler

run_art: build_art
	./art/ARTCompiler test/$(file).art test/$(file).stk
	$(MAKE) run_stk file=$(file)

build_imp: build_stk #build_art
	ocamlc -c -I utils/ utils/tagset.mli utils/tagset.ml # A virer
	ocamlc -c -I utils/ utils/ARTTree.mli utils/ARTTree.ml # Pareil
	ocamlc -c -I utils/ utils/IMPTree.mli utils/IMPTree.ml

clear:
	rm -rf stk/*.byte stk/*.cmo stk/*.cmi stk/*.ml stk/STKCompiler
	rm -rf utils/*.cmi utils/*.cmo
	rm -rf vm/*.byte vm/*.cmo vm/*.cmi vm/VM
	rm -rf assembler/*.byte assembler/*.cmo assembler/*.cmi assembler/Assembler
	rm -rf test/*.asm test/*.btc a.out
	rm -rf art/*.cmi art/*.cmx art/*.cmo art/*.o art/*a.out art/*.conflicts art/*.automaton art/ARTLexer.ml art/ARTParser.ml art/ARTParser.mli art/ARTCompiler	
