(**************************************************************************)
(*                        TIPE 2018 - Compiling                           *)
(*               Kineider    Bourret-Mathieu    Laborier                  *)
(*                                                                        *)
(*                  LEXICAL ANALYSIS WITH CAMLLEX TOOL                    *)
(*                                                                        *)
(*               This file is published under WTFPL License               *)
(**************************************************************************)

{
  #open "parser";;
  exception EOF;;
}

rule Token = parse
    [` `]       { Token lexbuf }
  | (`-`?)([`0`-`9`]+) { NUM(get_lexeme lexbuf) }
  | `+`         { PLUS }
  | `-`         { MOINS }
  | `*`         { FOIS }
  | `/`         { DIV }
  | `(`         { LPAR }
  | `)`         { RPAR }
  | eof         { raise EOF }
;;

