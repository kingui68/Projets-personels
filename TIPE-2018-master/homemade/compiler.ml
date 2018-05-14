#open "lex";;  (* string -> lexbuf *)
#open "synt";; (* lexbuf -> lextree *)
#open "make_code";; (* lextree -> string *)

(* compiler: string -> string
 *)
let compiler src =
	let rec comp_aux trees =
		match trees with
		| [] -> ""
		| t::others -> (comp_aux others) ^ (make_code t)
	in

	comp_aux (synt (lexing src))
;;
