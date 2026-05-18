# MiniImp Makefile
TARGET = tests/minimp_test
CFG_TARGET = tests/minimp_cfg_test
DATAFLOW_TARGET = tests/minimp_dataflow_test
OPT_TARGET = tests/minimp_opt_test
LLVM_TARGET = tests/minimp_llvm_test
FUN_TARGET = tests/minifun_test

LIB_MODULES = minimp_ast minimp_cfg minimp_dataflow minimp_opt minimp_cfg_dot minimp_eval minimp_lexer minimp_parser minimp_llvm

TEST_MODULES = $(LIB_MODULES) tests/minimp_test
CFG_MODULES = $(LIB_MODULES) tests/minimp_cfg_test
DATAFLOW_MODULES = $(LIB_MODULES) tests/minimp_dataflow_test
OPT_MODULES = $(LIB_MODULES) tests/minimp_opt_test
LLVM_MODULES = $(LIB_MODULES) tests/minimp_llvm_test

MINIFUN_LIB_MODULES = minifun_ast minifun_eval minifun_typechecker minifun_infer minifun_lexer minifun_parser
MINIFUN_TEST_MODULES = $(MINIFUN_LIB_MODULES) tests/minifun_test

LIB_CMXS = $(LIB_MODULES:%=%.cmx)
TEST_CMXS = $(TEST_MODULES:%=%.cmx)
CFG_CMXS = $(CFG_MODULES:%=%.cmx)
DATAFLOW_CMXS = $(DATAFLOW_MODULES:%=%.cmx)
OPT_CMXS = $(OPT_MODULES:%=%.cmx)
LLVM_CMXS = $(LLVM_MODULES:%=%.cmx)

MINIFUN_LIB_CMXS = $(MINIFUN_LIB_MODULES:%=%.cmx)
MINIFUN_TEST_CMXS = $(MINIFUN_TEST_MODULES:%=%.cmx)

.PHONY: all test clean dataflow opt llvm fun

all: $(TARGET)
	./$(TARGET)

test: $(TARGET)
	./$(TARGET)

cfg: $(CFG_TARGET)
	./$(CFG_TARGET)

dataflow: $(DATAFLOW_TARGET)
	./$(DATAFLOW_TARGET)

opt: $(OPT_TARGET)
	./$(OPT_TARGET)

llvm: $(LLVM_TARGET)
	./$(LLVM_TARGET)

fun: $(FUN_TARGET)
	./$(FUN_TARGET)

minimp_parser.ml minimp_parser.mli: minimp_parser.mly
	menhir --explain minimp_parser.mly

minimp_lexer.ml: minimp_lexer.mll
	ocamllex minimp_lexer.mll

minifun_parser.ml minifun_parser.mli: minifun_parser.mly
	menhir --explain minifun_parser.mly

minifun_lexer.ml: minifun_lexer.mll
	ocamllex minifun_lexer.mll

%.cmi: %.mli
	ocamlfind ocamlopt -package str -c $<

%.cmx: %.ml
	ocamlfind ocamlopt -package str -c $<

tests/minimp_test.cmx: minimp_ast.cmx minimp_cfg.cmx minimp_cfg_dot.cmx \
					   minimp_eval.cmx minimp_lexer.cmx minimp_parser.cmx minimp_dataflow.cmx
minimp_dataflow.cmx: minimp_cfg.cmx
tests/minimp_dataflow_test.cmx: minimp_ast.cmx minimp_cfg.cmx minimp_cfg_dot.cmx \
                       			minimp_eval.cmx minimp_lexer.cmx minimp_parser.cmx minimp_dataflow.cmx
minimp_cfg.cmx: minimp_ast.cmx
minimp_cfg_dot.cmx: minimp_cfg.cmx minimp_dataflow.cmx
minimp_eval.cmx: minimp_ast.cmx
minimp_lexer.cmx: minimp_parser.cmi
minimp_parser.cmx: minimp_parser.ml minimp_parser.mli minimp_ast.cmx
minimp_opt.cmx: minimp_dataflow.cmx minimp_ast.cmx minimp_cfg.cmx minimp_cfg_dot.cmx \
			   minimp_eval.cmx minimp_lexer.cmx minimp_parser.cmx
minimp_llvm.cmx: minimp_ast.cmx minimp_cfg.cmx
tests/minimp_llvm_test.cmx: minimp_ast.cmx minimp_cfg.cmx minimp_eval.cmx \
                             minimp_lexer.cmx minimp_parser.cmx minimp_llvm.cmx
tests/minimp_opt_test.cmx: minimp_dataflow.cmx minimp_ast.cmx minimp_cfg.cmx minimp_cfg_dot.cmx \
                           minimp_eval.cmx minimp_lexer.cmx minimp_parser.cmx

minifun_ast.cmx: minifun_ast.ml
minifun_eval.cmx: minifun_ast.cmx
minifun_typechecker.cmx: minifun_ast.cmx
minifun_infer.cmx: minifun_ast.cmx minifun_typechecker.cmx
minifun_lexer.cmx: minifun_parser.cmi
minifun_parser.cmx: minifun_parser.ml minifun_parser.mli minifun_ast.cmx
tests/minifun_test.cmx: minifun_ast.cmx minifun_eval.cmx minifun_typechecker.cmx \
                        minifun_infer.cmx minifun_lexer.cmx minifun_parser.cmx

$(TARGET): $(TEST_CMXS)
	ocamlfind ocamlopt -package str -linkpkg -o $@ $(TEST_CMXS)

$(CFG_TARGET): $(CFG_CMXS)
	ocamlfind ocamlopt -package str -linkpkg -o $@ $(CFG_CMXS)

$(DATAFLOW_TARGET): $(DATAFLOW_CMXS)
	ocamlfind ocamlopt -package str -linkpkg -o $@ $(DATAFLOW_CMXS)

$(OPT_TARGET): $(OPT_CMXS)
	ocamlfind ocamlopt -package str -linkpkg -o $@ $(OPT_CMXS)

$(LLVM_TARGET): $(LLVM_CMXS)
	ocamlfind ocamlopt -package str,unix -linkpkg -o $@ $(LLVM_CMXS)

$(FUN_TARGET): $(MINIFUN_TEST_CMXS)
	ocamlfind ocamlopt -package str -linkpkg -o $@ $(MINIFUN_TEST_CMXS)

clean:
	rm -f *.cm[iox] *.o *.txt \
	      minimp_parser.ml minimp_parser.mli minimp_parser.conflicts \
	      minimp_lexer.ml \
	      minifun_parser.ml minifun_parser.mli minifun_parser.conflicts \
	      minifun_lexer.ml \
		  tests/minimp_test tests/*.cmi tests/*.cmx tests/*.o tests/*.ll tests/*_bin \
		  $(CFG_TARGET) \
	      $(DATAFLOW_TARGET) \
	      $(OPT_TARGET) \
	      $(LLVM_TARGET) \
	      $(FUN_TARGET) \
	      *.dot *.png