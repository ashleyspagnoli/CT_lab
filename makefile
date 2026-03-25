# MiniImp Makefile
TARGET = tests/minimp_test

LIB_MODULES = minimp_ast minimp_cfg minimp_cfg_dot minimp_eval minimp_lexer minimp_parser

TEST_MODULES = $(LIB_MODULES) tests/minimp_test

LIB_CMXS = $(LIB_MODULES:%=%.cmx)
TEST_CMXS = $(TEST_MODULES:%=%.cmx)

.PHONY: all test clean

all: $(TARGET)
	./$(TARGET)

test: $(TARGET)
	./$(TARGET)

minimp_parser.ml minimp_parser.mli: minimp_parser.mly
	menhir --explain minimp_parser.mly

minimp_lexer.ml: minimp_lexer.mll
	ocamllex minimp_lexer.mll

%.cmi: %.mli
	ocamlfind ocamlopt -package str -c $<

%.cmx: %.ml
	ocamlfind ocamlopt -package str -c $<

minimp_cfg.cmx: minimp_ast.cmx
minimp_cfg_dot.cmx: minimp_cfg.cmx
minimp_eval.cmx: minimp_ast.cmx
minimp_lexer.cmx: minimp_parser.cmi
minimp_parser.cmx: minimp_parser.ml minimp_parser.mli minimp_ast.cmx
minimp_test.cmx: minimp_ast.cmx minimp_cfg.cmx minimp_cfg_dot.cmx \
                 minimp_eval.cmx minimp_lexer.cmx minimp_parser.cmx

$(TARGET): $(TEST_CMXS)
	ocamlfind ocamlopt -package str -linkpkg -o $@ $(TEST_CMXS)

clean:
	rm -f *.cm[iox] *.o \
	      minimp_parser.ml minimp_parser.mli minimp_parser.conflicts \
	      minimp_lexer.ml \
		  tests/minimp_test tests/*.cmi tests/*.cmx tests/*.o \
	      *.dot *.png