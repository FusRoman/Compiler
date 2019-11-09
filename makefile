build_asm:
	ocamlc -c -I utils/ utils/tagset.mli utils/tagset.ml
	ocamlc -c -I utils/ utils/arith.mli utils/arith.ml
	ocamlc -I utils/ utils/arith.cmo vm/VM.ml -o vm/VM
	ocamlc assembler/Assembler.ml -o assembler/Assembler

run_asm: build_asm
	@./assembler/Assembler test/$(file).asm
	@echo "\n\n\nExecuting $(file).btc:"
	@./vm/VM test/$(file).btc

build_stk_acc: 
	@$(MAKE) -s build_asm
	ocamlc -c -I utils/ utils/cycle.mli utils/cycle.ml
	ocamllex stk/STKCompiler.mll
	ocamlc -I utils/ utils/cycle.cmo stk/STKCompiler.ml -o stk/STKCompiler

build_stk: 
	@$(MAKE) -s build_asm
	ocamlc -c -I utils/ utils/cycle.mli utils/cycle.ml
	ocamllex stk/STKCompilerAlloc.mll
	ocamlc -I utils/ utils/cycle.cmo utils/tagset.cmo stk/STKCompilerAlloc.ml -o stk/STKCompilerAlloc

run_stk_acc: build_stk_acc
	@./stk/STKCompiler test/$(file).stk
	@$(MAKE) -s run_asm file=$(file)

run_stk: build_stk
	@./stk/STKCompilerAlloc test/$(file).stk
	@$(MAKE) -s run_asm file=$(file)

build_art: 
	@$(MAKE) -s build_stk
	menhir -v art/ARTParser.mly
	ocamlc -c -I utils/ utils/ARTTree.mli utils/ARTTree.ml
	ocamlc -c -I art/ -I utils/ art/ARTParser.mli art/ARTParser.ml
	ocamllex art/ARTLexer.mll
	ocamlc -c -I art/ art/ARTLexer.ml
	ocamlc -I utils/ -I art/ utils/cycle.cmo utils/arith.cmo utils/tagset.cmo utils/ARTTree.cmo art/ARTLexer.cmo art/ARTParser.cmo art/ARTCompiler.ml -o art/ARTCompiler

run_art: build_art
	@./art/ARTCompiler test/$(file).art test/$(file).stk
	@$(MAKE) -s run_stk file=$(file)

build_imp: 
	@$(MAKE) -s build_art
	ocamlc -c -I utils/ utils/IMPTree.mli utils/IMPTree.ml
	menhir -v imp/IMPParser.mly
	ocamlc -c -I imp/ -I utils/ imp/IMPParser.mli imp/IMPParser.ml
	ocamllex imp/IMPLexer.mll
	ocamlc -c -I imp/ imp/IMPLexer.ml
	ocamlc -I utils/ -I imp/ utils/cycle.cmo utils/arith.cmo utils/tagset.cmo utils/ARTTree.cmo utils/IMPTree.cmo imp/IMPLexer.cmo imp/IMPParser.cmo imp/IMPCompiler.ml -o imp/IMPCompiler

run_imp: build_imp
	@./imp/IMPCompiler test/$(file).imp
	@$(MAKE) -s run_art file=$(file)

build_interprete_var :
	menhir -v interprete_var/VARParser.mly
	ocamllex interprete_var/VARLexer.mll
	ocamlc -c interprete_var/Op.ml
	ocamlc -c -I interprete_var/ interprete_var/IMPExpr.ml
	ocamlc -c -I interprete_var/ interprete_var/FUNInstr.ml
	ocamlc -c -I interprete_var/ interprete_var/VAR.ml
	ocamlc -c -I interprete_var/ interprete_var/VARParser.mli interprete_var/VARParser.ml
	ocamlc -c -I interprete_var/ interprete_var/VARLexer.ml
	ocamlc -I interprete_var/ -c interprete_var/VAREvals.ml
	ocamlc -I interprete_var/ interprete_var/Op.cmo interprete_var/VARLexer.cmo interprete_var/VARParser.cmo interprete_var/VAREvals.cmo interprete_var/VARInterpreter.ml -o interprete_var/VARInterpreter

run_interprete_var: build_interprete_var
	./interprete_var/VARInterpreter test/$(file).var

build_cll:
	@$(MAKE) -s build_imp
	ocamlc -c -I utils/ utils/CLLTree.mli utils/CLLTree.ml
	menhir -v var/cll/CLLParser.mly
	ocamllex var/cll/CLLLexer.mll
	ocamlc -c -I var/cll/ -I utils/ var/cll/CLLParser.mli var/cll/CLLParser.ml
	ocamlc -c -I var/cll/ var/cll/CLLLexer.ml

clear:
	rm -rf stk/*.byte stk/*.cmo stk/*.cmi stk/*.ml stk/STKCompiler stk/STKCompilerAlloc
	rm -rf utils/*.cmi utils/*.cmo utils/*.cmx utils/*.o
	rm -rf vm/*.byte vm/*.cmo vm/*.cmi vm/VM
	rm -rf assembler/*.byte assembler/*.cmo assembler/*.cmi assembler/Assembler
	rm -rf test/*.asm test/*.btc a.out
	rm -rf art/*.cmi art/*.cmx art/*.cmo art/*.o art/*a.out art/*.conflicts art/*.automaton art/ARTLexer.ml art/ARTParser.ml art/ARTParser.mli art/ARTCompiler
	rm -rf imp/*.cmi imp/*.cmx imp/*.cmo imp/*.o imp/*a.out imp/*.conflicts imp/*.automaton imp/IMPLexer.ml imp/IMPParser.ml imp/IMPParser.mli imp/IMPCompiler
	rm -rf interprete_var/*.cmi interprete_var/*.cmo interprete_var/VARParser.conflicts interprete_var/VARParser.automaton
	rm -rf interprete_var/VARParser.ml interprete_var/VARParser.mli interprete_var/VARInterpreter interprete_var/VARLexer.ml
	rm -rf var/cll/*.cmi var/cll/*.cmo var/cll/*.o var/cll/CLLParser.ml var/cll/CLLParser.mli var/cll/CLLParser.automaton var/cll/CLLParser.conflicts var/cll/CLLLexer.ml