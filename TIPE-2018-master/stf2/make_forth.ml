

(* make_forth : string list -> string -> string
   make_forth info expr renvoie la définition de la fonction de corps expr et de nom et arguments contenus dans info etc.
*)

let make_forth info expr =
  let rec var_rev_str dat =
    match
      dat
    with
    | [] -> ""
    | x::r -> x^ " " ^var_rev_str r
  in
  (": " ^ hd info ^ " LOCALS| " ^ var_rev_str (tl info) ^ "| " ^ expr ^ " ; ") (* Ending space to avoid instructions to stick to the `;` character *)
;;
  
let app f values =
  values ^" "^ f
;;
  
