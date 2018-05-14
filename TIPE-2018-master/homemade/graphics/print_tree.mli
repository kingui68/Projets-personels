(***********************************************************************************)
(*                              TIPE 2018 - Compiling                              *)
(*                                 Nestor Laborier                                 *)
(*                                                                                 *)
(*                       TREE GRAPHIC SYNTHESIS MAIN FUNCTION                      *)
(*                               - INTERFACE FILE -                                *)
(*                                                                                 *)
(*                    This file is published under WTFPL License                   *)
(***********************************************************************************)

value lexemtree_to_stringtree : lexeme btree -> string btree ;;

value lexemtree_to_stringtree_list : lexeme btree list -> string btree list ;;

value print_tree : string -> () ;;
