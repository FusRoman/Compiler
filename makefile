run:
	menhir art/ARTParser.mly
	ocamlopt art/ART_SyntaxTree.mli
	ocamlopt -I art/ art/ART_SyntaxTree.ml
	ocamlopt -I art/ art/ARTParser.mli
	ocamlopt -I art/ art/ARTParser.ml
	ocamllex art/ARTLexer.mll
	ocamlopt -I art/ art/ARTLexer.ml
	ocamlopt -I art/ -o art/ARTCompiler art/ARTParser.cmx art/ART_SyntaxTree.cmx art/ARTLexer.cmx art/ARTCompiler.ml
	./art/ARTCompiler test/$(name_prog).art test/$(name_prog).stk
	ocamllex stk/STKCompiler.mll
	ocamlc stk/STKCompiler.ml -o stk/STKCompiler
	ocamlc -o assembler/Assembler assembler/Assembler.ml
	ocamlc -o vm/VM vm/VM.ml
	./stk/STKCompiler test/$(name_prog).stk
	./assembler/Assembler test/$(name_prog).asm
	./vm/VM test/$(name_prog).btc

ARTCompiler:
	menhir -v art/ARTParser.mly
	ocamlopt art/ART_SyntaxTree.mli
	ocamlopt -I art/ art/ART_SyntaxTree.ml
	ocamlopt -I art/ art/ARTParser.mli
	ocamlopt -I art/ art/ARTParser.ml
	ocamllex art/ARTLexer.mll
	ocamlopt -I art/ art/ARTLexer.ml
	ocamlopt -I art/ -o art/ARTCompiler art/ARTParser.cmx art/ART_SyntaxTree.cmx art/ARTLexer.cmx art/ARTCompiler.ml

clear:
	rm -rf stk/*.byte stk/*.cmo stk/*.cmi stk/*.ml stk/STKCompiler
	rm -rf vm/*.byte vm/*.cmo vm/*.cmi vm/VM
	rm -rf assembler/*.byte assembler/*.cmo assembler/*.cmi assembler/Assembler
	rm -rf test/*.asm test/*.btc a.out
	rm -rf art/*.cmi art/*.cmx art/*.o art/*a.out art/*.conflicts art/*.automaton art/ARTLexer.ml art/ARTParser.ml art/ARTParser.mli art/ARTCompiler	
