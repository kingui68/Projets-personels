(* DEFINITIONS *)

let lettres = [`a`; `b`; `c`; `d`; `e`; `f`; `g`; `h`; `i`; `j`; `k`; `l`; `m`; `n`; `o`; `p`; `q`; `r`; `s`; `t`; `u`; `v`; `w`; `x`; `y`; `z`; `A`; `B`; `C`; `D`; `E`; `F`; `G`; `H`; `I`; `J`; `K`; `L`; `M`; `N`; `O`; `P`; `Q`; `R`; `S`; `T`; `U`; `V`; `W`; `X`; `Y`; `Z`]
;;
let chiffres = [`1`; `2`; `3`; `4`; `5`; `6`; `7`; `8`; `9`]
;;
let char_spe = [`?`; `!`; `.`; `+`; `-`; `*`; `/`; `<`; `=`; `>`; `;`; `$`; `%`; `^`; `&`; `_`; `~`; `@`] 
;;
(* sigma est l'alphabet composé des charactères acceptés par scheme *)
let sigma = lettres @ chiffres @ char_spe
;;
let keyword = [|"define"; "set"; "+"; "-"; "/"; "*"|]
;;
(* type d'automates finis déterministe
Note : le nombre d'état n'est pas très utile, on peut l'avoir avec la longueur de finals... mais je préfère faire ainsi *)
type automate = {
      etats: int; (* etats représentent le nombre d'états, ils sont représentés par les entiers [0,...,etats -1] *)
      transition: int -> char -> int;
      finals: bool vect (* finals.[i] contient true si i est un état final, false sinon *)
   }
;;



(* FONCTIONS AUXILIAIRES *)

(* prefixe s t renvoie true si s est un prefixe de t, false sinon*)
let prefixe s t = compare_strings s t = - 2
;;

(* split u retourne la liste des charactères de u apparaissant dans l'ordre *)
let split u =
   let n = string_length u in
   let l = ref [] in
   for i = 0 to n - 1 do
      l := u.[i] :: (!l)
   done;
   rev !l
;;

(* sépare la chaine u donnée en argument à chaque espace, transforme ces sous-chaînes en listes (via split) et en retourne la liste dans l'ordre de leur apparition dans u *)
let strip u =
   let l = split u in
   let res = ref [] in
   let rec aux l acc = match l with
         | [] -> res := acc :: (!res)
         | x :: r when x = ` ` or x = `\t` or x = `\n` ->
               begin res := acc :: (!res); aux r [] end
         | x :: r -> aux r (x :: acc)
   in aux l [] ;
   rev (map rev !res )
;;
(* Exemples : *)
strip "fbdgd769\tzrr\nresf" ;;
strip "988**^ uhqa/\t" ;;







(* DEFINITIONS DES AUTOMATES *)



(* foncton de transition de l'automate id, renvoie -1 si l'automate bloque *)
let trans_id q a =
   if (q = 0 && (mem a lettres)) or (q = 1 && (mem a sigma))
   then 1
   else - 1
;;

(* automate reconnaissant les identifiants i.e. les noms de variables.
Ils sont de la forme lettres.(sigma)* *)
let id = {
      etats = 1;
      transition = trans_id;
      finals = [|false; true|]
   }
;;



(* fonction de transition de l'automate nb, renvoie -1 si l'automate bloque *)
let trans_nb q a =
let ch = mem a chiffres in
   if q = 0 && a = `-` then 1
   else if (q = 0 or q = 1) && (a = `.`) then 2
   else if (q = 0 or q = 1 or q=3) && ch then 3
   else if (q = 2 or q = 4) && ch then 4
   else if q=3 && a=`.` then 5
   else if (q=5 or q=6) && ch then 6 
   else -1
;;

(* automate reconnaissant les nombres de scheme. 
Plus précisément un nombre est de la forme :
 (- + eps).chiffres.chiffres*.(`.` + eps).chiffres*
 ou (- + eps).`.`chiffres.chiffres*
 avec eps le mot vide et . la concaténation.
 Autrement dit les nombres peuvent etre précédé d'un - et à virgule (noté . ).
 Les nombres 42. et .42 sont reconnus mais pas . *)
let nb = {
      etats = 7;
      transition = trans_nb;
      finals = [|false; false; false; true; true; true; true|]
   }
;;






(* FONCTIONS PRINCIPALES *)

(* reconnait A u retourne true si l'automate A reconnait le mot u (sous forme de chaîne de charactères) *)
let reconnait A u =
   let delta = A.transition in
   let l = split u in
   let rec aux l q = match l with
         | [] -> not (q = - 1) && A.finals.(q)
         | x :: r -> let q' = delta q x in
               aux r q' && not (q' = - 1)
   in aux l 0
;;
(* Exemples *)
reconnait nb "98635" ;;
reconnait nb "-546";;
reconnait nb "849.84635";;
reconnait nb "-484.446";;
reconnait nb "8476.";;
reconnait nb "-8448.";;
reconnait nb ".8965";;
reconnait nb "-.8845";;
reconnait nb ".";;
reconnait nb "-8845f846";;


