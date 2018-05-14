(**************************************************************************)
(*                        TIPE 2018 - Compiling                           *)
(*               Kineider    Bourret-Mathieu    Laborier                  *)
(*                                                                        *)
(*                      GENERATED LEXICAL ANALYSIS                        *)
(*                                                                        *)
(*               This file is published under WTFPL License               *)
(**************************************************************************)


#open "obj";;
#open "lexing";;


  #open "parser";;
  exception EOF;;

let rec action_8 lexbuf = ((
 raise EOF ) : 'Token)
and action_7 lexbuf = ((
 RPAR ) : 'Token)
and action_6 lexbuf = ((
 LPAR ) : 'Token)
and action_5 lexbuf = ((
 DIV ) : 'Token)
and action_4 lexbuf = ((
 FOIS ) : 'Token)
and action_3 lexbuf = ((
 MOINS ) : 'Token)
and action_2 lexbuf = ((
 PLUS ) : 'Token)
and action_1 lexbuf = ((
 NUM(get_lexeme lexbuf) ) : 'Token)
and action_0 lexbuf = ((
 Token lexbuf ) : 'Token)
and state_0 lexbuf =
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_9 lexbuf
 |  `/` -> action_5 lexbuf
 |  `-` -> state_7 lexbuf
 |  `+` -> action_2 lexbuf
 |  `*` -> action_4 lexbuf
 |  `)` -> action_7 lexbuf
 |  `(` -> action_6 lexbuf
 |  ` ` -> action_0 lexbuf
 |  `\000` -> action_8 lexbuf
 |  _ -> backtrack lexbuf
and state_7 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_3;
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_9 lexbuf
 |  _ -> backtrack lexbuf
and state_9 lexbuf =
  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
  lexbuf.lex_last_action <- magic action_1;
  match get_next_char lexbuf with
    `9`|`8`|`7`|`6`|`5`|`4`|`3`|`2`|`1`|`0` -> state_9 lexbuf
 |  _ -> backtrack lexbuf
and Token lexbuf =
  start_lexing lexbuf;
  (state_0 lexbuf : 'Token)
;;
