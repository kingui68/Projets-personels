
#open "dbg";;
#open "lexm";;
#open "print_lextree";;
      

(* make_code: lextree -> string
   Construct and return the code described by the provided lextree.
 *)
let rec make_code (ltree:lextree) =
  dbgbgn "make_code";

  (* Forth language constructors *)
  let forth_op op l r = (* Operator: +, -, ... *)
    ((make_code l)^" "^(make_code r)^" "^op^" ")
  and forth_num nb = (* Integer *)
    string_of_int nb
  and forth_var_get vb = (* Variable: get value *)
    vb ^ " @ " (* Forth fetch *)
  and forth_var_set name valtree = (* Variable: assignation *)
  (make_code valtree) ^ " " ^ name ^ " ! "
  and forth_var_decl name valtree = (* Variable: declaration *)
  "variable " ^ name ^ " " ^ (make_code valtree) ^ " " ^ name ^ " " ^ " ! "
  in

  (* Used language constructors *)
  (* Possible to define other outputs... *)
  let op = forth_op
  and num = forth_num
  and var_get = forth_var_get
  and var_set = forth_var_set
  and var_decl = forth_var_decl
  in

  match
    ltree
  with
  | Empty -> ""
  | Node(ls,PLUS,rs) -> (op "+" ls rs)  (* Should "OP of string" in lexeme definition? *)
  | Node(ls,MINUS,rs) -> (op "-" ls rs) (* Node(l,OP(symbol),r) -> (op symbol l r) *)
  | Node(ls,TIMES,rs) -> (op "*" ls rs)
  | Node(ls,DIV,rs) -> (op "/" ls rs)
  | Node(Empty,NUM(nb),Empty) -> (num nb)
  | Node(Empty,VAR(vb),Empty) -> (var_get vb)
  | Node(Node(Empty,VAR(vb),Empty), SET,rs) -> (var_set vb rs)
  | Node(Node(Empty,VAR(vb),Empty), DEF, rs) -> (var_decl vb rs)
  | _ -> dbg "Error in lextree structure:"; print_lextree ltree; print_newline (); failwith "Error in lextree structure"
;;
