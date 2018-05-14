(* 
Analyse lexicale
 *)

{
  #open "parser";;
  #open "infos_fonction";;
  exception EOF;;
}

rule Token = parse
    [` ``\t``\n`] 		{ Token lexbuf }                (* motifs ignorés *)
  | [`0`-`9`]+     	        { NUM(get_lexeme lexbuf) }	(* entier non signé *)
  | `(`         		{ LPAR }
  | `)`         		{ RPAR }
  | "define" " (" [`a`-`z``+``-``*``/`]+ (` `([`a`-`z`]+))* `)` { DEF(infos_fonction (get_lexeme lexbuf)) } (* infos_fonctions renvoie une str contenant "nom_de_fct arg1 .. argn". *)
  | [`a`-`z``+``-``*``/`]+      { WRD(get_lexeme lexbuf) }
  | eof         		{ raise EOF }
;;
