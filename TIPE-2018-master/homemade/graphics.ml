(***********************************************************************************)
(*                              TIPE 2018 - Compiling                              *)
(*                                 Nestor Laborier                                 *)
(*                                                                                 *)
(*                     GRAPHICS MODULE FOR TREE REPRESENTATION                     *)
(*                                                                                 *)
(*                                                                                 *)
(*                    This file is published under WTFPL License                   *)
(***********************************************************************************)


load_object "graphics";;
  #open "graphics";;
  #open "lex";;
  #open "lexm";;    

(* sample trees *)

(*               *
               /   \
   a =        +     b
             / \
            4   3                        *)

let a = Node(
	    Node(
		Node(Empty,"4",Empty),
		"+",
		Node(Empty,"3",Empty)),
	    "*",
	    Node(Empty, "b", Empty))
;;

let b = Node( Node(Empty,"6",Empty),
	      "+",
	      Node(Node(Empty, "9", Empty),
		   "*",
		   Node(Empty, "4", Empty)
		  )
	    )
;;
  
(* end of samples *)  

(* these variables are the maximum dimensions
   for the window, if the tree is too big, larger
   than these dimensions, the programm will
   automaticaly resize the tree *)  
let default_height = 720 ;; 
let default_width = 1280;;
let default_node_radius = 100;;
let default_text_width = let _ = open_graph "" in
			 let t = fst(text_size "a") in
			 let _ = close_graph ()in
			 t;;
		       
(* tree_depth : 'a btree -> int
   returns the depth of the tree *)
let rec tree_depth a = match a with
  | Empty -> 0
  | Node(x1,x,x2) -> 1 + max (tree_depth x1) (tree_depth x2)
;;

(* these are the currents dimensions of the window, before the
   programm resizes it *)  
let max_width a = int_of_float (2. ** float_of_int (tree_depth a - 1));;

let node_radius = ref default_node_radius;; (* current radius for the node *)
  
let height = ref default_height;;    (* current height of the window *)

let width = ref default_width;;	  (* current width of the window *)


(* height_of_tree : 'a btree -> unit 
   set the current height of the window to a more accurate value
   if the tree is too big for the default size, it reduces the node size *)
let height_of_tree a = if (!node_radius * 2 * tree_depth a) <= (default_height - 50)
		       then height := (!node_radius * 2 * tree_depth a) + 50
		       else (node_radius := (((default_height - 50)/(tree_depth a))/2);
			     height := 720)
;;

(* width_of_tree : 'a btree -> unit
   set the current width of the window to a more accurate value
   if the tree is too big for the default size, it reduces the node size *)  
let width_of_tree a = if  (!node_radius * 2 * max_width a) <= (default_width - 80)
		      then width := (!node_radius * 2 * max_width a) + 80
		      else (node_radius := (((default_width - 80) / (max_width a))/2);
			    width := 1280)
;;

(*
let text_size_of_tree a = if 10 * default_text_width > !node_radius
			  then !node_radius / 10
			  else default_text_width;;
*)

(* create the arguments for make_window *)
let make_size a = " " ^ string_of_int(!width) ^ "x" ^ string_of_int(!height);;

(* current radius *)
let effective_radius = ref !node_radius;;
  
(* make_window : 'a btree -> unit
   creates a window in adapted size for the tree *)  
let make_window a = height_of_tree a;
		    width_of_tree a;
		    effective_radius := !node_radius - (!node_radius / 3);
		    open_graph (make_size a);
		    set_line_width 3;;

(* create_node : string -> int -> int -> unit 
   creates a node str on the coordinates (x,y) in the intialized window 
!!!NEEDS TO BE ADAPTED TO CENTER THE TEXT *)
let create_node str x y = draw_circle x y !effective_radius;
			  moveto x y;
			  draw_string str;;

(* linking_nodes : int -> int -> int -> int -> unit
   linking_nodes draw a line between node (x1,y1) and (x2,y2) *)
let linking_nodes x1 y1 x2 y2 =
  let a1,b1 = float_of_int x1, float_of_int y1 in
  let a2,b2 = float_of_int x2, float_of_int y2 in
  let ab = ((a2 -. a1) ** 2. +. (b2 -. b1) ** 2.) ** 0.5 in
  let r = float_of_int(!effective_radius) in
  let dx = float_of_int(abs(x2 - x1)) in
  let dy = float_of_int(abs(y2 - y1)) in
  let ex = (dx *. r) /. ab in
  let ey = (r ** 2. -. ex ** 2.) ** 0.5 in
  let a3,b3,a4,b4 =
    if y1 > y2
    then if x1 > x2
	 then a1-.ex,b1-.ey,a2+.ex,b2+.ey
	 else a1+.ex,b1-.ey,a2-.ex,b2+.ey
    else if x1 > x2
         then a1-.ex,b1+.ey,a2+.ex,b2-.ey
         else a1+.ex,b1+.ey,a2-.ex,b2-.ey
  in
  moveto (int_of_float a3) (int_of_float b3);
  lineto (int_of_float a4) (int_of_float b4)
;;

(* auxilary function to implement a timeout
   wait : int -> ()
   wait n just waits approximately n seconds *)  
let wait =
  let time = ref 0. in
  fun delay ->
  while sys__time () -. !time < delay do done;
  time := sys__time ()
;;

(* draw_tree : 'a btree -> ()
   opens the graphic window, it draws recursively the tree
   beginning on the top, finishing with the bottom *)
let draw_tree a =
  make_window a;
  let rec draw_rec a x_root y_root left_lim right_lim =
    let shift = (right_lim - left_lim) / 4 in
    let right_x = x_root + shift in
    let left_x = x_root - shift in
    let son_y = y_root - (2 * !node_radius) in
    match a with
    |Empty -> ()
    |Node(Empty,str,Empty) -> create_node str x_root y_root
    |Node(l_son,str,r_son) -> begin
			      create_node str x_root y_root;
			      wait 1.5;
			      linking_nodes x_root y_root left_x son_y ;
			      linking_nodes x_root y_root right_x son_y ;
			      draw_rec l_son left_x son_y left_lim x_root;
			      draw_rec r_son right_x son_y x_root right_lim
			    end
  in
  draw_rec a (!width / 2) (!height - !node_radius) 0 !width
;;
