
(* python-like '*^' operator for strings (multiple concatenation) *)
let rec prefix *^ str n =
  if n=0
  then ""
  else( str^(str*^(n-1)) )
;;

(* charlist_of_string: string -> char list
   Build and return the list of the characters contained in s, in order of appearance.
   charlist_of_string "caml" returns [`c`;`a`;`m`;`l`].
 *)
let charlist_of_string s =
  let slen = string_length s in
  let rec charlst_aux i =
    if i=slen
    then []
    else (nth_char s i)::charlst_aux (i+1)
  in
  charlst_aux 0
;;


(* Common schars/ochars *)
let schars = charlist_of_string "+-*/()";; (* bubble characters *)
let ochars = [` `;`\t`;`\n`];; (* blanks to skip *)


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
