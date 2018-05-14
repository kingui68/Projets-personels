#open "lexm";;

(* python-like '*^' operator for strings (multiple concatenation) *)
let rec prefix *^ str n =
  if n=0
  then ""
  else( str^(str*^(n-1)) )
;;

let print_lexeme lex =
  print_string (string_of_lexeme lex)
;;

(* print_lextree: lextree -> int -> unit()
   Prints the lextree lt to the console with an initial offset of lv.
 *)
let rec print_lextree_lv lextree lv =
  match
    lextree
  with
  | lexm__Empty -> ()
  | lexm__Node( lt, lex, rt ) ->
     print_newline ();
     print_string (":" *^ lv); (* offset string: ":" *)
     print_lexeme lex;
     print_lextree_lv lt (lv+1);
     print_lextree_lv rt (lv+1)
;;

(* print_lextree: lextree -> unit
   print a lextree (lexeme btree) to the std output.
 *)
let print_lextree lt =
  print_lextree_lv lt 0;
  flush std_out
;;

let print_list_of_lextree lst =
  match lst with
  | [] -> ()
  | x::r -> print_newline (); print_lextree x; print_newline ();
;;

let print_list l pr_elt =

  let rec print_content lst =
    match lst with
	| x::r -> pr_elt x; if(r<>[])then(print_string "; "); print_content r
	| [] -> ()
  in

  print_string "[";
  print_content l;
  print_string "]"
;;
