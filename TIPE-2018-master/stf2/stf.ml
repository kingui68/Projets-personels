#open "sys";;


(* récupération du nom de fichier en sortie depuis la ligne de commande *)
let getOutName( argv ) =
  if( vect_length argv >= 2 )
  then( argv.(1) )
  else(
    print_string "Attention: pas de fichier en sortie";
    "default_out.forth"
  )
;;


(* récupération du nom de fichier en entrée depuis la ligne de commande *)
let getInName( argv ) =
  if( vect_length argv >= 3 )
  then( argv.(2) )
  else(
    print_string "Attention: pas de fichier en entrée";
    "default_in.scheme"
  )
;;




(* Utilisation de l'éxécutable:
   
   ./stf fichier_sortie fichier_entrée
   
   Si pas de fichier spécifié en sortie/entrée, default_in/out utilisé
*)

let ch_out = open_out (getOutName sys__command_line) in (* canal de sortie (fichier forth) *)
let ch_in = open_in (getInName sys__command_line) in    (* canal d'entrée (fichier scheme) *)
try
  let lexbuf = lexing__create_lexer_channel ch_in in (* *)
  while true do
    let result = parser__Main lexer__Token lexbuf in (* attrape le résultat en forth *)
    output_string ch_out result; (* envoi dans le fichier *)
  done;
with
| Eof ->
  flush ch_out;
  close_out ch_out;
  close_in  ch_in;
;;
