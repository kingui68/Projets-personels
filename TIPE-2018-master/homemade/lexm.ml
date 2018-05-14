

(* Scheme parsed lexemes.
 *)
type lexeme =
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | EOF
  | NUM of int
  | VAR of string
  | LPAR (* ( *)
  | RPAR (* ) *)
  | DEF
  | SET
;;

type lexbuf == lexeme list;;

type 'a btree =
  | Empty
  | Node of 'a btree * 'a * 'a btree
;;

type lextree == lexeme btree ;;

let tree_of_lexeme lex = Node (Empty,lex,Empty) ;;
let treelist_of_list l = map tree_of_lexeme l ;;

let string_of_lexeme lex =
  match lex with
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | DIV -> "/"
  | EOF -> "EOF"
  | NUM(nb) -> string_of_int nb
  | VAR(vb) -> vb
  | LPAR -> "("
  | RPAR -> ")"
  | DEF -> "DEF"
  | SET -> "SET"
;;
