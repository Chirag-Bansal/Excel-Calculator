all:
	ocamllex lexer.mll       # generates lexer.ml
	ocamlc -c functions.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c calc.ml
	ocamlc -o ass4 str.cma functions.cmo lexer.cmo parser.cmo calc.ml

clean:
	rm -rf ass4 *.cmi *.cmo *.mli lexer.ml parser.ml