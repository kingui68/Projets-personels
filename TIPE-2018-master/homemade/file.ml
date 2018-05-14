

(* Return the string following the first -i flag.
 *)
let rec getInFileName argv =
  match
    argv
  with
  | "-i"::fileIn::_ -> fileIn
  | x::r -> getInFileName r
  | [] -> "in.scheme"
;;

(* Return the string following the first -o flag.
 *)
let rec getOutFileName argv =
  match
    argv
  with
  | "-o"::fileOut::_ -> fileOut
  | x::r -> getOutFileName r
  | [] -> "out.forth"
;;

(* Multiplicative string concatenation. *)
let rec prefix *^ str n =
  if(n=0)
  then( "" )
  else( str ^ (str *^ (n-1) ) )
;;

(* readWholeFile: string -> string
  Read an entire file and return its content.
*)
let readWholeFile filename =
  let file = open_in filename in
  let size = in_channel_length file in
  let s = " " *^ (size-1) in
  let _ = input file s 0 (size-1) in
  close_in file;
  s
;;


let writeToFile filename s =
	let file = open_out filename in
	output_string file s;
	close_out file;
;;
