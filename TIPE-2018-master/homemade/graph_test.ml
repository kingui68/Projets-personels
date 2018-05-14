load_object "graphics";;
#open "graphics";;

let a = Node(
	    Node(
		Node(Empty,"4",Empty),
		"+",
		Node(Empty,"3",Empty)),
	    "*",
	    Node(Empty, "b", Empty))
;;

let default_height = 720 ;; 
let default_width = 1280;;
let default_node_size = 100;;
		       
  
type 'a btree =
  | Empty
  | Node of 'a btree * 'a * 'a btree
;;

(* tree_depth : 'a btree -> int
   returns the depth of the tree *)
let rec tree_depth a = match a with
  | Empty -> 0
  | Node(x1,x,x2) -> 1 + max (tree_depth x1) (tree_depth x2)
;;

let max_width a = int_of_float (2. ** float_of_int (tree_depth a - 1));;

let node_size = ref default_node_size;; (* current size for the node *)
  
let height = ref default_height;;    (* current height of the window *)

let width = ref default_width;;	  (* current width of the window *)


(* height_of_tree : 'a btree -> unit 
   set the current height of the window to a more accurate value
   if the tree is too big for the default size, it reduces the node size *)
let height_of_tree a = if (!node_size * tree_depth a) <= (default_height - 20)
		       then height := (!node_size * tree_depth a) + 20
		       else (node_size := ((default_height - 20) / (tree_depth a)) ;
			     height := 720)
;;

(* width_of_tree : 'a btree -> unit
   set the current width of the window to a more accurate value
   if the tree is too big for the default size, it reduces the node size *)  
let width_of_tree a = if  (!node_size * max_width a) <= (default_width - 40)
		      then width := (!node_size * max_width a) + 40
		      else (node_size := ((default_width - 40) / (max_width a)) ;
			    width := 1280)
;;

let make_size a = " " ^ string_of_int(!width) ^ "x" ^ string_of_int(!height);;

(* make_window : 'a btree -> unit
   creates a window in adapted size for the tree *)  
let make_window a = height_of_tree a;
		    width_of_tree a;
		    open_graph (make_size a);;


