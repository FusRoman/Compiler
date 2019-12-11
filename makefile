
# ------------------------------ Langage ASM ---------------------------------------

build_asm:
	ocamlc -c -I utils/ utils/tagset.mli utils/tagset.ml
	ocamlc -c -I utils/ utils/arith.mli utils/arith.ml
	ocamlc -I utils/ utils/arith.cmo vm/VM.ml -o vm/VM
	ocamlc assembler/Assembler.ml -o assembler/Assembler

run_asm_inter:
	@./assembler/Assembler test/$(file).asm
	@echo "\n\n\nExecuting $(file).btc:"
	@./vm/VM test/$(file).btc

run_asm: build_asm
	@$(MAKE) run_asm_inter file=$(file)


# ------------------------------ Langage STK ---------------------------------------

build_stk: 
	@$(MAKE) -s build_asm
	ocamlc -c -I utils/ utils/cycle.mli utils/cycle.ml
	ocamllex stk/STKCompilerAlloc.mll
	ocamlc -I utils/ utils/cycle.cmo utils/tagset.cmo stk/STKCompilerAlloc.ml -o stk/STKCompilerAlloc

run_stk_inter:
	@./stk/STKCompilerAlloc test/$(file).stk
	@$(MAKE) -s run_asm_inter file=$(file)

run_stk: build_stk
	@$(MAKE) run_stk_inter file=$(file)


# ------------------------------ Langage ART ---------------------------------------

build_art: 
	@$(MAKE) -s build_stk
	menhir -v art/ARTParser.mly
	ocamlc -c -I utils/ -I art/ art/ARTTree.mli art/ARTTree.ml
	ocamlc -c -I art/ -I utils/ art/ARTParser.mli art/ARTParser.ml
	ocamllex art/ARTLexer.mll
	ocamlc -c -I art/ art/ARTLexer.ml
	ocamlc -I utils/ -I art/ utils/cycle.cmo utils/arith.cmo utils/tagset.cmo art/ARTTree.cmo \
		art/ARTLexer.cmo art/ARTParser.cmo art/ARTCompiler.ml \
		-o art/ARTCompiler

run_art_inter:
	@./art/ARTCompiler test/$(file).art test/$(file).stk
	@$(MAKE) -s run_stk_inter file=$(file)

run_art: build_art
	@$(MAKE) -s run_art_inter file=$(file)


# ------------------------------ Langage IMP ---------------------------------------

build_imp: 
	@$(MAKE) -s build_art
	ocamlc -c -I utils/ -I art/ -I imp/ imp/IMPTree.mli imp/IMPTree.ml
	menhir -v imp/IMPParser.mly
	ocamlc -c -I utils/ -I art/ -I imp/ imp/IMPParser.mli imp/IMPParser.ml
	ocamllex imp/IMPLexer.mll
	ocamlc -c -I imp/ imp/IMPLexer.ml
	ocamlc -I utils/ -I art/ -I imp/ utils/cycle.cmo utils/arith.cmo utils/tagset.cmo \
		art/ARTTree.cmo imp/IMPTree.cmo imp/IMPLexer.cmo imp/IMPParser.cmo imp/IMPCompiler.ml \
		-o imp/IMPCompiler

run_imp_inter:
	@./imp/IMPCompiler test/$(file).imp
	@$(MAKE) -s run_art_inter file=$(file)

run_imp: build_imp
	@$(MAKE) -s run_imp_inter file=$(file) 


# ------------------------------ Interpreteur VAR ----------------------------------

build_var_interpreter:
	menhir -v interprete_var/VARParser.mly
	ocamllex interprete_var/VARLexer.mll
	ocamlc -c interprete_var/Op.ml
	ocamlc -c -I interprete_var/ interprete_var/IMPExpr.ml
	ocamlc -c -I interprete_var/ interprete_var/FUNInstr.ml
	ocamlc -c -I interprete_var/ interprete_var/VAR.ml
	ocamlc -c -I interprete_var/ interprete_var/VARParser.mli interprete_var/VARParser.ml
	ocamlc -c -I interprete_var/ interprete_var/VARLexer.ml
	ocamlc -I interprete_var/ -c interprete_var/VAREvals.ml
	ocamlc -I interprete_var/ interprete_var/Op.cmo interprete_var/VARLexer.cmo interprete_var/VARParser.cmo interprete_var/VAREvals.cmo \
		interprete_var/VARInterpreter.ml -o interprete_var/VARInterpreter

run_var_interpreter: build_interprete_var
	./interprete_var/VARInterpreter test/$(file).var


# ------------------------------ Langage CLL ---------------------------------------

build_cll: 
	@$(MAKE) -s build_imp
	ocamlc -c -I utils/ -I art/ -I imp/ -I var/cll/ var/cll/CLLTree.mli var/cll/CLLTree.ml
	menhir -v var/cll/CLLParser.mly
	ocamllex var/cll/CLLLexer.mll
	ocamlc -c -I utils/ -I art/ -I imp/ -I var/cll/ var/cll/CLLParser.mli var/cll/CLLParser.ml
	ocamlc -c -I var/cll/ var/cll/CLLLexer.ml
	ocamlc -I utils/ -I art/ -I imp/ -I var/cll/ utils/cycle.cmo utils/arith.cmo utils/tagset.cmo \
		art/ARTTree.cmo imp/IMPTree.cmo var/cll/CLLTree.cmo var/cll/CLLLexer.cmo var/cll/CLLParser.cmo var/cll/CLLCompiler.ml \
		-o var/cll/CLLCompiler

run_cll_inter:
	@./var/cll/CLLCompiler test/$(file).cll
	@$(MAKE) -s run_imp_inter file=$(file)

run_cll: build_cll
	@$(MAKE) -s run_cll_inter file=$(file)


# ------------------------------ Langage FUN ---------------------------------------

build_fun:
	@$(MAKE) -s build_cll
	ocamlc -c -I utils/ -I art/ -I imp/ -I var/cll/ -I var/fun/ var/fun/FUNTree.mli var/fun/FUNTree.ml
	menhir -v var/fun/FUNParser.mly
	ocamllex var/fun/FUNLexer.mll
	ocamlc -c -I utils/ -I art/ -I imp/ -I var/cll/ -I var/fun/ var/fun/FUNParser.mli var/fun/FUNParser.ml
	ocamlc -c -I var/fun/ var/fun/FUNLexer.ml
	ocamlc -I utils/ -I art/ -I imp/ -I var/cll/ -I var/fun/ \
		utils/cycle.cmo utils/arith.cmo utils/tagset.cmo \
		art/ARTTree.cmo imp/IMPTree.cmo var/cll/CLLTree.cmo var/fun/FUNTree.cmo \
		var/fun/FUNLexer.cmo var/fun/FUNParser.cmo var/fun/FUNCompiler.ml \
		-o var/fun/FUNCompiler

run_fun_inter:
	@./var/fun/FUNCompiler test/$(file).fun
	@$(MAKE) -s run_cll_inter file=$(file)

run_fun: build_fun
	@$(MAKE) -s run_fun_inter file=$(file)


# ------------------------------ Langage VAR ---------------------------------------

build_var :
	@$(MAKE) -s build_fun
	ocamlc -c -I utils/ -I art/ -I imp/ -I var/cll/ -I var/fun/ -I var/var/ \
		var/var/VARTree.mli var/var/VARTree.ml
	menhir -v var/var/VARParser.mly
	ocamllex var/var/VARLexer.mll
	ocamlc -c -I utils -I art/ -I imp/ -I var/cll/ -I var/fun/ -I var/var/ \
		var/var/VARParser.mli var/var/VARParser.ml
	ocamlc -c -I var/var/ var/var/VARLexer.ml
	ocamlc -I utils/ -I art/ -I imp/ -I var/cll/ -I var/fun/ -I var/var/ \
		utils/cycle.cmo utils/arith.cmo utils/tagset.cmo \
		art/ARTTree.cmo imp/IMPTree.cmo var/cll/CLLTree.cmo var/fun/FUNTree.cmo var/var/VARTree.cmo \
		var/var/VARLexer.cmo var/var/VARParser.cmo var/var/VARCompiler.ml -o var/var/VARCompiler

run_var_inter:
	@./var/var/VARCompiler test/$(file).var
	@$(MAKE) -s run_fun_inter file=$(file)

run_var: build_var
	@$(MAKE) -s run_var_inter file=$(file)


# ------------------------------ Langage TPL ---------------------------------------

build_tpl:
	@$(MAKE) -s build_var
	ocamlc -c -I utils/ -I art/ -I imp/ -I var/cll/ -I var/fun/ -I var/var/ -I typ/tpl/ \
		typ/tpl/TPLTree.mli typ/tpl/TPLTree.ml
	ocamlc -I utils/ -I art/ -I imp/ -I var/cll/ -I var/fun/ -I var/var/ -I typ/tpl/ -o typ/tpl/TPLCompiler \
		utils/arith.cmo utils/tagset.cmo utils/cycle.cmo art/ARTTree.cmo imp/IMPTree.cmo \
		var/cll/CLLTree.cmo var/fun/FUNTree.cmo var/var/VARTree.cmo \
		var/var/VARParser.cmo var/var/VARLexer.cmo typ/tpl/TPLTree.cmo \
		typ/tpl/TPLCompiler.ml

run_tpl_inter:
	@./typ/tpl/TPLCompiler test/$(file).tpl
	@$(MAKE) -s run_var_inter file=$(file)

run_tpl: build_tpl
	@$(MAKE) -s run_var_inter file=$(file)

# ------------------------------ Langage TYP ---------------------------------------

build_typ: 
	@$(MAKE) -s build_tpl
	ocamlc -c -I utils/ -I art/ -I imp/ -I var/cll/ -I var/fun/ -I var/var/ -I typ/tpl/ -I typ/typ/ \
		typ/typ/TYPTree.mli typ/typ/TYPTree.ml
	menhir -v typ/typ/TYPParser.mly
	ocamllex typ/typ/TYPLexer.mll
	ocamlc -c -I utils -I art/ -I imp/ -I var/cll/ -I var/fun/ -I var/var/ -I typ/tpl/ -I typ/typ/ \
		typ/typ/TYPParser.mli typ/typ/TYPParser.ml
	ocamlc -c -I typ/typ/ typ/typ/TYPLexer.ml

	ocamlc -I utils/ -I art/ -I var/var/ -I typ/typ/ utils/tagset.cmo utils/cycle.cmo utils/arith.cmo \
		art/ARTTree.cmo imp/IMPTree.cmo var/cll/CLLTree.cmo var/fun/FUNTree.cmo typ/typ/TYPTree.cmo \
		typ/typ/TYPLexer.cmo typ/typ/TYPParser.cmo var/var/VARTree.cmo -o typ/typ/TYPCompiler \
		typ/typ/TYPCompiler.ml

# ------------------------------ Langage CLS ---------------------------------------

build_cls:
	@$(MAKE) -s build_typ
	ocamlc -c -I utils/ -I art/ -I imp/ -I var/cll/ -I var/fun/ -I var/var/ -I typ/tpl/ -I typ/typ/ -I typ/cls/ \
		typ/cls/CLSTree.mli typ/cls/CLSTree.ml
	menhir -v typ/cls/CLSParser.mly
	ocamllex typ/cls/CLSLexer.mll
	ocamlc -c -I utils -I art/ -I imp/ -I var/cll/ -I var/fun/ -I var/var/ -I typ/tpl/ -I typ/typ/ -I typ/cls/ \
		typ/cls/CLSParser.mli typ/cls/CLSParser.ml
	ocamlc -c -I typ/cls/ typ/typ/CLSLexer.ml

	ocamlc -I utils/ -I art/ -I var/var/ -I typ/typ/ -I typ/cls/ \
		utils/tagset.cmo utils/cycle.cmo utils/arith.cmo \
		art/ARTTree.cmo imp/IMPTree.cmo var/cll/CLLTree.cmo var/fun/FUNTree.cmo typ/typ/TYPTree.cmo typ/cls/CLSTree.cmo \
		typ/cls/CLSLexer.cmo typ/cls/CLSParser.cmo var/var/VARTree.cmo -o typ/cls/CLSCompiler \
		typ/cls/CLSCompiler.ml 

# ------------------------------ ChainCompiler -------------------------------------

build_chain_compiler: 
	@$(MAKE) -s build_typ
	#@$(MAKE) -s build_cls
	ocamlc -o compiler/ChainCompiler \
		-I utils/ -I art/ -I imp/ -I var/cll/ -I var/fun/ -I var/var/ -I typ/tpl/ -I typ/typ/ \
		\ # -I typ/cls/ \
		utils/tagset.cmo utils/cycle.cmo utils/arith.cmo \
		art/ARTTree.cmo art/ARTParser.cmo art/ARTLexer.cmo \
		imp/IMPTree.cmo imp/IMPParser.cmo imp/IMPLexer.cmo \
		var/cll/CLLTree.cmo var/cll/CLLParser.cmo var/cll/CLLLexer.cmo \
		var/fun/FUNTree.cmo var/fun/FUNParser.cmo var/fun/FUNLexer.cmo \
		var/var/VARTree.cmo var/var/VARParser.cmo var/var/VARLexer.cmo \
		typ/tpl/TPLTree.cmo \
		typ/typ/TYPTree.cmo typ/typ/TYPParser.cmo typ/typ/TYPLexer.cmo \
		\ # typ/cls/CLSTree.cmo typ/cls/CLSParser.cmo typ/cls/CLSLexer.cmo \
		compiler/ChainCompiler.ml


# ------------------------------ CLEAR ---------------------------------------------

clear:
	rm -rf utils/*.cmi utils/*.cmo utils/*.cmx utils/*.o
	rm -rf vm/*.byte vm/*.cmo vm/*.cmi vm/VM
	rm -rf assembler/*.byte assembler/*.cmo assembler/*.cmi assembler/Assembler

	rm -rf stk/*.byte stk/*.cmo stk/*.cmi stk/*.ml stk/STKCompiler stk/STKCompilerAlloc
	rm -rf test/stk/*.asm test/stk/*.btc a.out

	rm -rf art/*.cmi art/*.cmx art/*.cmo art/*.o art/*a.out art/*.conflicts art/*.automaton art/ARTLexer.ml art/ARTParser.ml art/ARTParser.mli art/ARTCompiler
	rm -rf test/art/*.stk test/art/*.asm test/art/*.btc

	rm -rf imp/*.cmi imp/*.cmx imp/*.cmo imp/*.o imp/*a.out imp/*.conflicts imp/*.automaton imp/IMPLexer.ml imp/IMPParser.ml imp/IMPParser.mli imp/IMPCompiler
	rm -rf test/imp/*.art test/imp/*.stk test/imp/*.asm test/imp/*.btc

	rm -rf interprete_var/*.cmi interprete_var/*.cmo interprete_var/VARParser.conflicts interprete_var/VARParser.automaton
	rm -rf interprete_var/VARParser.ml interprete_var/VARParser.mli interprete_var/VARInterpreter interprete_var/VARLexer.ml

	rm -rf var/cll/*.cmi var/cll/*.cmo var/cll/*.o var/cll/CLLParser.ml var/cll/CLLParser.mli var/cll/CLLParser.automaton var/cll/CLLParser.conflicts var/cll/CLLLexer.ml var/cll/CLLCompiler
	rm -rf test/cll/*.imp test/cll/*.art test/cll/*.stk test/cll/*.asm test/cll/*.btc

	rm -rf var/fun/*.cmi var/fun/*.cmo var/fun/*.o var/fun/FUNParser.ml var/fun/FUNParser.mli var/fun/FUNParser.automaton var/fun/FUNParser.conflicts var/fun/FUNLexer.ml var/fun/FUNCompiler
	rm -rf test/fun/*.btc test/fun/*.asm test/fun/*.stk test/fun/*.art test/fun/*.imp test/fun/*.cll

	rm -rf var/var/*.cmi var/var/*.cmo var/var/*.o var/var/VARParser.ml var/var/VARParser.mli var/var/VARParser.automaton var/var/VARParser.conflicts var/var/VARLexer.ml var/var/VARCompiler
	rm -rf test/var/*.btc test/var/*.asm test/var/*.stk test/var/*.art test/var/*.imp test/var/*.cll test/var/*.fun

	rm -rf typ/tpl/*.cmi typ/tpl/*.cmo var/var/*.o var/var/TPLCompiler 
	rm -rf test/tpl/*.var test/tpl/*.fun test/tpl/*.cll test/tpl/*.imp test/tpl/*.art test/tpl/*.stk test/tpl/*.asm test/tpl/*.btc

	rm -rf typ/typ/*.cmi typ/typ/*.cmo typ/typ/*.o typ/typ/TYPParser.ml typ/typ/TYPParser.mli typ/typ/TYPParser.automaton typ/typ/TYPParser.conflicts typ/typ/TYPLexer.ml typ/typ/TYPCompiler
	rm -rf test/typ/*.btc test/typ/*.asm test/typ/*.stk test/typ/*.art test/typ/*.imp test/typ/*.cll test/typ/*.fun test/typ/*.var test/typ/*.tpl

	rm -rf typ/cls/*.cmi typ/cls/*.cmo typ/cls/*.o typ/cls/CLSParser.ml typ/cls/CLSParser.mli typ/cls/CLSParser.automaton typ/cls/CLSParser.conflicts typ/cls/CLSLexer.ml typ/cls/CLSCompiler
	rm -rf test/cls/*.btc test/cls/*.asm test/cls/*.stk test/cls/*.art test/cls/*.imp test/cls/*.cll test/cls/*.fun test/cls/*.var test/cls/*.tpl test/cls/*.typ

	rm -rf compiler/*.cmi compiler/*.cmo compiler/ChainCompiler