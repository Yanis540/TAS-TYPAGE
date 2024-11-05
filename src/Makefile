ML = ocamllex
YACC_ML = ocamlyacc
OCAMLC = ocamlc



test : eval type ./test/test_ast.ml  ./test/test_type.ml   ./test/test_ast_alpha.ml  ./test/test_ast_subs.ml ./test/test_ast_reduction.ml ./test/test_type_unification.ml ./test/test_ast_ref.ml
	$(OCAMLC) -o ./exec/test_ast ast.cmo eval.cmo  ./test/test_ast.ml
	$(OCAMLC) -o ./exec/test_type ast.cmo eval.cmo type.cmo  ./test/test_type.ml 
	$(OCAMLC) -o ./exec/test_ast_alpha ast.cmo eval.cmo type.cmo  ./test/test_ast_alpha.ml 
	$(OCAMLC) -o ./exec/test_ast_subs ast.cmo eval.cmo type.cmo  ./test/test_ast_subs.ml 
	$(OCAMLC) -o ./exec/test_ast_reduction ast.cmo eval.cmo type.cmo  ./test/test_ast_reduction.ml 
	$(OCAMLC) -o ./exec/test_type_unification ast.cmo eval.cmo type.cmo  ./test/test_type_unification.ml 
	$(OCAMLC) -o ./exec/test_ast_ref ast.cmo eval.cmo type.cmo  ./test/test_ast_ref.ml 
eval: ast eval.ml  
	$(OCAMLC) -o ./exec/eval  ast.cmo eval.ml  
ast: ast.ml  
	$(OCAMLC) -o ./exec/ast  ast.ml  

type: ast eval type.ml  
	$(OCAMLC) -o ./exec/type ast.cmo  eval.cmo  type.ml  

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f prologTerm
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml
	rm *~