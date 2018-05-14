
#open "dbg";;
#open "lexm";;
#open "print_lextree";;

(* liste l de lexème -> taleau t de lexème
  calcule du tableau codant la profondeur des éléments de t
  convertit les éléments de t en arbre de lexème
 ( On determine le max du tableau des profondeurs
  On remplace l'opération de prof max
  On actualise la profondeur )
*)

let rec max_list l = match l with
  |[]-> failwith "La liste est vide"
  |[x]-> x
  |x::r -> max x (max_list r)
;;


let vect_max t =
  let l = list_of_vect t in
  max_list l
;;

let max_index t m =
  let rec boucle t m i =
    if t.(i) = m then i else boucle t m (i+1)
  in boucle t m 0
;;

let slice t i j =
  dbgbgn ("slice [i:"^string_of_int i^" j:"^string_of_int j^"]");
  let t' = make_vect (j-i) t.(0) in (* blaise: changé j en j-i *)
  dbg "slice access indexes:\n";
  for k=i to j-1 do (* blaise: changé j en j-1 (erreur systématique car borne atteinte) *)
    dbg ("[t'.("^string_of_int (k-i)^") <- t.("^string_of_int k^")]\n");
    t'.(k-i)<-t.(k)
  done ;
  dbgend "slice";
  t'
;;

let break_instruction l  =
  dbgbgn "break_instruction";
  let t = vect_of_list l in
  let n = vect_length t in
  dbg ("vect size: "^string_of_int n^"\n");
  let p = make_vect n (-1) in
  let c = ref 0 in
  for i=0 to n-1 do
    if t.(i) = LPAR then c:= !c +1
    else if t.(i) = RPAR then c:= !c -1
    else () ;
    p.(i)<-(!c) ;
  done ;

  dbg "creating list of instructions...\n";
  let instr = ref [] in
  let expr_bgn = ref 0 in
  for i=0 to n-1 do
  dbg ("looping... "^string_of_int i ^ "\n");
    if p.(i) == 0 then (
		(instr :=  (list_of_vect (slice t !expr_bgn (i+1))):: !instr); (* edit blaise: changé i en i+1 (il manquait un caractère) *)
		expr_bgn := i+1
		)
	(* aussi enlevé c: il était modifié mais pas utilisé... *)
  done ;
  dbg "done.\n\n";
  !instr
;;



let profondeur_init t =
  dbgbgn "profondeur_init";
  let n = vect_length t in
  let p = make_vect n (-1) in
  let c = ref 0 in
  let m = ref 0 in
  for i=0 to n-1 do
    if t.(i) = LPAR then c:= !c +1
    else if t.(i) = RPAR then c:= !c -1
    else () ;
    begin
      if !c > !m then m:=!c else ()
    end ;
    p.(i)<-(!c)

  done ;

  dbg ("profondeur calculée:\n");
  for i=0 to (n-2) do
    dbg (string_of_lexeme t.(i)^", ")
  done;
  dbg (string_of_lexeme t.(n-1));
  dbg "\n";
  for i=0 to (n-2) do
    dbg (string_of_int p.(i)^", ")
  done;
  dbg (string_of_int p.(n-1)^", ");
  dbg ("\n");

  (p,!m,n)
;;

let remplace t i n =
  dbgbgn ("remplace [i:"^ (string_of_int !i) ^" n:"^ string_of_int n ^"]");
  try
  let a = match t.(!i+1) with
    |Empty -> dbg "arbre vide.\n"; failwith "l'arbre est vide"
    |Node(_,x,_)->x
  in
  let N= Node(t.(!i+2),a,t.(!i+3)) in
  let t'= make_vect (n-4) (Node(Empty,PLUS,Empty)) in
  for k = 0 to !i-1 do
    t'.(k)<- t.(k)
  done ;
  t'.(!i)<-N ;
  for k = !i+1 to n-5 do
    t'.(k)<-t.(k+4)
  done ;
  t'
  with
  | vect_assign -> dbg "Vect assign error"; (failwith "")
;;

let prof_actualisation p =
  dbgbgn "prof_actualisation";

  let m = vect_max p in
  let i = max_index p m in
  let p' = make_vect ((vect_length p)-4) 0 in
  if i <> 0
  then
    for k = 0 to i-1 do
    p'.(k)<- p.(k)
    done
  else () ;
  p'.(i)<-(m-1) ;
  for k = i+1 to (vect_length p')-1 do
    p'.(k) <-p.(k+4)
  done ;
  p'
;;

let synt (l: lexbuf) =
  dbgbgn "synt";

  let list_instruct = break_instruction l in

  let synt_aux inst =
    dbgbgn "synt_aux";
	dbg "inst: ";
	if do_debug then (print_list inst print_lexeme; print_newline ());
    let t = vect_of_list inst in
    let a,b,c =  profondeur_init t in
    let p = ref a in
    let m = ref b in
    let n = ref c in
    let t' = ref (vect_of_list (treelist_of_list inst)) in

    while vect_length !t' <> 1 && c<>0 do
      t' := remplace !t' (ref (max_index !p !m)) !n ;
      p := prof_actualisation !p ;
      m := vect_max !p ;
      n := vect_length !t'
    done ;
    (!t'.(0): lextree) in

  dbg "mapping to list of trees...\n";
  map synt_aux list_instruct
;;
