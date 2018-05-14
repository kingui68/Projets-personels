all: main
	make clean

main:
	camlc -o hmstf dbg.ml lexm.ml print_lextree.ml split_string.ml lex.ml synt.ml make_code.ml compiler.ml file.ml main.ml

clean:
	rm -f a.out
	rm -f compiler.zi compiler.zo file.zi file.zo dbg.zi dbg.zo
	rm -f lexm.zi lexm.zo lexer.zi lexer.zo lex.zi lex.zo
	rm -f print_lextree.zi print_lextree.zo
	rm -f main.zi main.zo
	rm -f make_code.zi make_code.zo
	rm -f split_string.zi split_string.zo
	rm -f synt.zi synt.zo

cleanall:
	rm -f hmstf
	make clean
