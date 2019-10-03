run:
	menhir -v art/ARTParser.mly
	ocamlopt art/ART_SyntaxTree.mli
	ocamlopt -I art/ art/ART_SyntaxTree.ml
	ocamlopt -I art/ art/ARTParser.mli
	ocamlopt -I art/ art/ARTParser.ml
	ocamllex art/ARTLexer.mll
	ocamlopt -I art/ art/ARTLexer.ml
	ocamlopt -I art/ -o art/ARTCompiler art/ARTParser.cmx art/ART_SyntaxTree.cmx art/ARTLexer.cmx art/ARTCompiler.ml
	./art/ARTCompiler test/$(name_prog).art test/$(name_prog).stk
	ocamllex -o stk/STKCompiler.byte stk/STKCompiler.mll
	ocamlc -o stk/ocaml/STKCompiler.byte stk/ocaml/STKCompiler.ml
	ocamlc -o assembler/Assembler.byte assembler/Assembler.ml
	ocamlc -o vm/VM.byte vm/VM.ml
	./stk/ocaml/STKCompiler.byte test/$(name_prog).stk
	./assembler/Assembler.byte test/$(name_prog).asm
	./vm/VM.byte test/$(name_prog).btc

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
	rm stk/*.byte
	rm stk/ocaml/*.byte
	rm stk/ocaml/*.cmo
	rm stk/ocaml/*.cmi
	rm vm/*.byte
	rm vm/*.cmo
	rm vm/*.cmi
	rm assembler/*.byte
	rm assembler/*.cmo
	rm assembler/*.cmi
	rm test/*.asm
	rm test/*.btc

clear_art:
	rm -rf art/*.cmi art/*.cmx art/*.o art/*a.out art/*.conflicts art/*.automaton art/ARTLexer.ml art/ARTParser.ml art/ARTParser.mli art/ARTCompiler
	
