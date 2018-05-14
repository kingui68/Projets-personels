(**************************************************************************)
(*                        TIPE 2018 - Compiling                           *)
(*               Kineider    Bourret-Mathieu    Laborier                  *)
(*                                                                        *)
(*                      SCHEME-TO-FORTH  COMPILER                         *)
(*                           MAIN  PROGRAMM                               *)
(*                                                                        *)
(*               This file is published under WTFPL License               *)
(**************************************************************************)



#open "sys";;

  
let getOutName( argv ) =
  if( vect_length argv >= 2 )
  then( argv.(1) )
  else( failwith "pas de fichier en sortie" )
;;


let getInName( argv ) =
  if( vect_length argv >= 3 )
  then( argv.(2) )
  else( failwith "pas de fichier en entree" )
;;
  
  
  
let canal_sortie = open_out (getOutName sys__command_line) in
let canal_entree = open_in (getInName sys__command_line) in			
try
  let lexbuf = lexing__create_lexer_channel canal_entree in
  while true do
    let result = parser__Main lexer__Token lexbuf in
    output_string canal_sortie result ;
    flush canal_sortie;
  done;
with
| Eof -> close_out canal_sortie;
;;



