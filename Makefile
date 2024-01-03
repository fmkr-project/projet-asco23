all: usage

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

lexer2.ml: lexer2.mll
	ocamllex $<

decoupe: lexer2.cmo decoupe_final.cmo
	ocamlc -o $@ $^

parser.ml: parser.mly
	ocamlyacc -v $<

parser.cmo: ast.cmi parser.cmi

parser.cmi: parser.mli ast.cmo
	ocamlc -c $<

parser: ast.cmo parser.cmo lexer2.cmo parser_final.cmo
	ocamlc -o $@ $^

usage:
	@echo "Taper make decoupe ou make parser"

clean:
	-rm lexer2.ml parser.ml parser.mli *.cmo *.cmi decoupe parser

ast.cmo: ast.cmi
decoupe_final.cmo : lexer2.cmo
parser_final.cmo : parser.cmi lexer2.cmo ast.cmi
