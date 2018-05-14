(* 
Analyse lexicale
 *)

{
  #open "parser";;
  exception EOF;;
}

rule Token = parse
    [` ``\t``\n`] 		{ Token lexbuf } 			(* motifs ignorés *)
  | (`-`?)([`0`-`9`]+)	{ NUM(get_lexeme lexbuf) }	(* entier signé *)
  | `+`        			{ PLUS }
  | `-`         		{ MOINS }
  | `*`         		{ FOIS }
  | `/`         		{ DIV }
  | `(`         		{ LPAR }
  | `)`         		{ RPAR }
  | eof         		{ raise EOF }
;;
