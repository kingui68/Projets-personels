(**************************************************************************)
(*                        TIPE 2018 - Compiling                           *)
(*               Kineider    Bourret-Mathieu    Laborier                  *)
(*                                                                        *)
(*                          LEXICAL ANALYSIS                              *)
(*                           MAIN  PROGRAMM                               *)
(*                                                                        *)
(*               This file is published under WTFPL License               *)
(**************************************************************************)

#open "dbg";;
#open "split_string";;

(*  int_or_string returns a number if string is a number, min_int otherwise
    THE MOST IMPORTANT FUNCTION IN THIS FILE *)
let int_or_string string =
  try int_of_string string
  with Failure "int_of_string" -> min_int
;;


(* Main fucntion of the lexical analysis
   lexing : string -> lexbuf
   Execute the lexical analysis on buffer.
   split_string rip the buffer into buf_med, it contains recognisable strings, it throw away the spaces and keeps only the good
   Then lexing executes a filtering on this buf_med
 *)
let lexing buffer =
  dbgbgn "lexing";
  let schars = [`+`; `-`; `*`; `/`; `(`; `)`] in   (*the recongized lexems/characters that induct separations between lexems *)
  let ochars = [` `;`\n`;`\t`] in                            (*the ignored characters that induct separations between lexems *)
  let buf_med = split_string buffer schars ochars in   (* splits the buffer into a list of recongnisables strings *)
  let rec lexing_aux buf (lexlist:lexm__lexbuf) =
    match buf with
    | [] -> lexlist
    | x::r -> begin
	      let temp = lexing_aux r lexlist in
		  dbg ("Matched lexem: "^x^"\n");
	      match x with
	      | "+" -> lexm__PLUS::temp
	      | "-" -> lexm__MINUS::temp
	      | "*" -> lexm__TIMES::temp
	      | "/" -> lexm__DIV::temp
	      | "(" -> lexm__LPAR::temp
	      | ")" -> lexm__RPAR::temp
	      | "define" -> lexm__DEF::temp
	      | "set!" -> lexm__SET::temp
	      | _ -> let m = int_or_string x in
		     if m = min_int
		     then (lexm__VAR x)::temp
		     else (lexm__NUM m)::temp
	    end
  in (lexing_aux buf_med ([]:lexm__lexbuf) : lexm__lexbuf)
;;
