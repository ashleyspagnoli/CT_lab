# MiniImp Makefile
TARGET = tests/minimp_test
DATAFLOW_TARGET = tests/minimp_dataflow_test

LIB_MODULES = minimp_ast minimp_cfg minimp_dataflow minimp_cfg_dot minimp_eval minimp_lexer minimp_parser

TEST_MODULES = $(LIB_MODULES) tests/minimp_test

DATAFLOW_MODULES = $(LIB_MODULES) tests/minimp_dataflow_test

LIB_CMXS = $(LIB_MODULES:%=%.cmx)
TEST_CMXS = $(TEST_MODULES:%=%.cmx)
DATAFLOW_CMXS = $(DATAFLOW_MODULES:%=%.cmx)

.PHONY: all test clean dataflow

all: $(TARGET)
	./$(TARGET)

test: $(TARGET)
	./$(TARGET)

dataflow: $(DATAFLOW_TARGET)
	./$(DATAFLOW_TARGET)

minimp_parser.ml minimp_parser.mli: minimp_parser.mly
	menhir --explain minimp_parser.mly

minimp_lexer.ml: minimp_lexer.mll
	ocamllex minimp_lexer.mll

%.cmi: %.mli
	ocamlfind ocamlopt -package str -c $<

%.cmx: %.ml
	ocamlfind ocamlopt -package str -c $<

minimp_dataflow.cmx: minimp_cfg.cmx
tests/minimp_dataflow_test.cmx: minimp_ast.cmx minimp_cfg.cmx minimp_cfg_dot.cmx \
                       			minimp_eval.cmx minimp_lexer.cmx minimp_parser.cmx minimp_dataflow.cmx
minimp_cfg.cmx: minimp_ast.cmx
minimp_cfg_dot.cmx: minimp_cfg.cmx
minimp_eval.cmx: minimp_ast.cmx
minimp_lexer.cmx: minimp_parser.cmi
minimp_parser.cmx: minimp_parser.ml minimp_parser.mli minimp_ast.cmx
tests/minimp_test.cmx: minimp_ast.cmx minimp_cfg.cmx minimp_cfg_dot.cmx \
                       minimp_eval.cmx minimp_lexer.cmx minimp_parser.cmx minimp_dataflow.cmx

$(TARGET): $(TEST_CMXS)
	ocamlfind ocamlopt -package str -linkpkg -o $@ $(TEST_CMXS)

$(DATAFLOW_TARGET): $(DATAFLOW_CMXS)
	ocamlfind ocamlopt -package str -linkpkg -o $@ $(DATAFLOW_CMXS)

clean:
	rm -f *.cm[iox] *.o *.txt \
	      minimp_parser.ml minimp_parser.mli minimp_parser.conflicts \
	      minimp_lexer.ml \
		  tests/minimp_test tests/*.cmi tests/*.cmx tests/*.o \
	      $(DATAFLOW_TARGET) \
	      *.dot *.png
