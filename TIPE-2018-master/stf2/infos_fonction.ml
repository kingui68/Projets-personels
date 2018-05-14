(* split_string: string -> char list -> char list -> string list
   Splits a string according to a list of split-chars and a list of split-omit-chars:
   schars characters are made a bubble between the left & right string while ochars are only a mark of slice and are then discarded.
   schar:'+' -> "a+b" -> "a";"+";"b"
   ochar:' ' -> "a b" -> "a";"b"

   each split_string call has at most 2 calls to sub_string which is a waste of time
   and could be optimized but it works good with small strings: try it with
   split_string ("z"*^10000) [] (charlist_of_string "0123456789abcdefghijklmnopqrstuvwxyz");;
 *)
let split_string str schars ochars =
  let slen = string_length str in

  (* split_aux: string -> int -> string list
     Perform a step in string-splitting operation on s at character n°i.
   *)
  let rec split_aux s i =
    if( s="" ) (* May call with empty string if arg ending with an (s/o)char *)
    then( [] )
    else(
      let c = nth_char s i in
      let cinsch,cinoch = (mem c schars),(mem c ochars) in

      if( cinsch || cinoch )
      then(
      	let okstr  = sub_string s 0 i in (* The word to keep *)
	let remstr = sub_string s (i+1) (string_length s-i-1) in (* the remainder of the program *)
	if( cinsch )
	(* if okstr="" don't cons it: [... ; "" ; ...] is BAD *)
	then(
	  if( okstr<>"" )
	  then( okstr::(string_of_char c)::(split_aux remstr 0) )
	  else( (string_of_char c)::(split_aux remstr 0) )
	)
	else(
	  if( okstr<>"" )
	  then( okstr::(split_aux remstr 0) )
	  else( (split_aux remstr 0) )
	)
      )
      else if i>=(string_length s)-1
      then [s] (* reached end of word. *)
      else(
	(* continue reading current word. *)
	split_aux s (i+1)
      )
    )
  in

  split_aux str 0
;;


(* infos_fonction : string -> string list
   Si s contient un début de définition de fonction scheme du type:
      "define (f a1 .. an)"
   infos_fonction s renvoie [f;a1;..;an]
 *)
let infos_fonction s =
  split_string (sub_string s 8 (string_length s - 9)) [] [` `]
;;


(*
  
  
(* member s char renvoie true si char est dans s et false sinon *)
let member s char =
  let a = try index_char s char with
	  |Not_found -> -1
  in
  not a = -1
;;
  
(* cut s start stop renvoie la sous-chaine commençant
 à start et finissant à stop-1 de s *)
let cut s start stop =
  let s'= ref "" in      
  for k = start to stop-1 do
    s' := !s' ^ (string_of_char s.[k])
  done
  ;
    !s'
;;
  

let info s =
  let n = string_length s in
  let lpar = index_char s "(".[0] in
  let space = ref lpar in
  let l = ref [] in
  let next = ref 0 in
  while member (cut s !space n) " ".[0] do
    next := index_char_from s  (!space + 1) " ".[0]  ;
    l:=(cut s (!space+1) !next):: (!l)
  done ;
  !l
;;
      	     	  
 *)
