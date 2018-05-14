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
	  
let infos s =
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
      	     	  
