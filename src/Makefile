ML = ocamllex
YACC_ML = ocamlyacc
OCAMLC = ocamlc



test : ast type ./test/test_ast.ml  ./test/test_type.ml 
	$(OCAMLC) -o ./exec/test_ast ast.cmo  ./test/test_ast.ml
	
	$(OCAMLC) -o ./exec/test_type ast.cmo type.cmo  ./test/test_type.ml 
ast: ast.ml  
	$(OCAMLC) -o ./exec/ast  ast.ml  
type: ast type.ml  
	$(OCAMLC) -o ./exec/type ast.cmo  type.ml  

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f prologTerm
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml
	rm *~