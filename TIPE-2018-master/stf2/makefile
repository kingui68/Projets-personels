
all:
	camlc -c infos_fonction.ml
	camlc -c make_forth.ml
	camlyacc parser.mly
	camlc -c parser.mli
	camlc -c parser.ml
	camllex lexer.mll
	camlc -c lexer.ml
	camlc -c stf.ml
	camlc -o stf infos_fonction.zo make_forth.zo lexer.zo parser.zo stf.zo


clean:
	rm -f parser.ml parser.mli parser.zi parser.zo
	rm -f lexer.ml lexer.zi lexer.zo 
	rm -f stf.zi stf.zo
	rm -f make_forth.zo make_forth.zi
	rm -f infos_fonction.zo infos_fonction.zi
