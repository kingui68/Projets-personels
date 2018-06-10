

(* Principes
On souhaite identifier les lexèmes d'une chaine de charactères.
Tout ces lexèmes peuvent être décrits par des expressions rationnelles et sont donc reconnaissables par des automates déterministes.
Dans un premier temps un construit un arbre pour chacune de ces expressions rationnelles.C
Ensuite on réalise l'automate produit (non déterministe). Plus précisément on simule la construction d'un automate produit pour éviter de construire tous les états. Dans les faits, on fait fonctionner tous les automates en parallèles sur la chaîne de charactères et on s'arrête juste avant que tous les automates ne bloquent. 
On a alors la liste des automates qui reconnaissent le plus long lexème, il suffit alors de définir un ordre de priorité pour choisir l'action.
Ici l'ordre est défini par l'ordre d'apparition des automates dans l'automate produit.
(ex : let peut être considéré comme le mot clé let ou l'identifiant let)
 *)  

(* La fonction finale est analyse_lexicale qui prend un mot sous forme de chaîne de charactères et retourne la liste des lexèmes *)

(* Pour que cette analyse lexicale reconnaisse de nouveaux mots clés, et avec la bonne priorité il suffit de les rajouter au bon endroit dans le tableau keyword *)






(* DEFINITIONS *)

let lettres = [`a`; `b`; `c`; `d`; `e`; `f`; `g`; `h`; `i`; `j`; `k`; `l`; `m`; `n`; `o`; `p`; `q`; `r`; `s`; `t`; `u`; `v`; `w`; `x`; `y`; `z`; `A`; `B`; `C`; `D`; `E`; `F`; `G`; `H`; `I`; `J`; `K`; `L`; `M`; `N`; `O`; `P`; `Q`; `R`; `S`; `T`; `U`; `V`; `W`; `X`; `Y`; `Z`]
;;
let chiffres = [`1`; `2`; `3`; `4`; `5`; `6`; `7`; `8`; `9`; `0`] (* EDIT NESTOR : il manquait le zero, je l'ai rajouté *)
;;
let char_spe = [`?`; `!`; `.`; `+`; `-`; `*`; `/`; `<`; `=`; `>`; `;`; `$`; `%`; `^`; `&`; `_`; `~`; `@`] (* EDIT NESTOR : je sais pas si on doit mettre ou pas la virgule *)
(* EDIT GUILLAUME : Cette liste est en réalité la liste des charactères qui peuvent apparaître dans un nom de variable. Je l'ai récupéré sur wikipédia... La virgule par exemple ne fait donc pas partie de cette liste. *)
;;
(* sigma est l'alphabet composé des charactères acceptés par scheme *)
let sigma = lettres @ chiffres @ char_spe
;;
(* Tableau des mots_clefs reconnus *)
let keyword = [|"define";"(";")";"set";"if";"then";"else"|]  (* EDIT NESTOR : je ne comprends pas pourquoi on doit utiliser un vect au lieu d'un list ici, ca doit
                                                                etre pour la suite que je n'ai pas encore lue ^^ *)
(* EDIT GUILLAUME : leur nombre n'est pas très grand donc la place en mémoire n'est pas très problématique... Les fonctions étaient juste plus simple à écrire avec un tableau ^^ *)
	       
;;
let operations = [`+`; `-`; `*`; `/`]
;;
(* type d'automates finis déterministes
Note : le nombre d'états n'est pas très utile, on peut l'avoir avec la longueur de finals... mais je préfère faire ainsi *)
type automate = {
      etats: int; (* etats représente le nombre d'états, ils sont représentés par les entiers [0,...,etats -1] *)
      transition: int -> char -> int;
      finals: bool vect ;(* finals.[i] contient true si i est un état final, false sinon *)
      lexeme : string (* représente l'"action" : permet d'associer le nom du lexème *)
   }
;;










(* FONCTIONS AUXILIAIRES *)


(* affiche le vecteur d'entiers donner en argument *)
let print_vect t =
   print_string "[|";
   do_vect (fun x -> begin print_int x; print_string "; " end) t;
   print_string "|]"
;;

(* prefixe s t renvoie true si s est un prefixe de t, false sinon
note : cette fonction s'avère inutile *)
let prefixe s t = compare_strings s t = - 2  (* EDIT NESTOR : compare_strings est une fonction de base Caml ? *)
;;                                           (* RE-EDIT NESTOR : c'est bon guillaume m'a expliqué ! *)
                                             (* RE-RE-EDIT NESTOR : et puis de toute facon on s'en sert pas bezef *)


(* split u retourne la liste des charactères de u apparaissant dans l'ordre *)
let split u =
   let n = string_length u in
   let l = ref [] in
   for i = 0 to n - 1 do
      l := u.[i] :: (!l)
   done;
   rev !l
;;


(* EDIT NESTOR : *)
(* split_rec u fait la même chose que split u mais en récursif, je sais pas si c'est mieux ... *)
(* EDIT GUILLAUME : Futé, mais honnêtement je ne pense pas que ce soit très important *) 
let split_rec u =
  let n = string_length u in
  let rec split_aux u i =
    if i = n then []
    else u.[i] :: (split_aux u (i+1))
  in split_aux u 0
;;

(* EDIT NESTOR : J'ai juste mis un peu en forme la fonction parce que j'arrivais pas à lire à cause de mon écran et de mes yeux pourris xD *)
(* reconnait A u retourne true si l'automate A reconnait le mot u (sous forme de chaîne de charactères)
Utile uniquement pour la programmation *)
let reconnait A u =
   let delta = A.transition in
   let l = split u in
   let rec aux l q =
     match l with
     | [] -> not (q = - 1) &&
	begin try A.finals.(q) with
	| nth_char -> false
	end
     | x :: r -> let q' = delta q x in
		 not (q' = - 1) && aux r q'
   in aux l 0
;;

(* EDIT NESTOR : si on met deux espaces ou deux séparations à la suite, ca crée des sous-chaines vides ... et du coup des listes de caractères vides, c'est grave ? *)
(* EDIT GUILLAUME : en effet cela crée des listes vides mais cela n'a aucune importance... (notamment pour la complexité) *)
(* RE-EDIT NESTOR : ne vaudrait-il pas mieux utiliser une liste de caractères séparant ? genre pour inclure le point-virgule ou la virgule ? *)
(* Ceux qu j'ai mis sont les seules autorisées en Scheme normalement... pas de virgule ou de points-virgules comme séparateurs en Scheme. Il est cependant possible (mais vraisemblablement inutile) soit d'enlever les listes vides après, soit de construire un automate reconnaissant les concaténations de séparateurs  (genre "\t  \n") *)
(* EDIT GUILLAUME : J'ai néanmoins omis de reconnaitre et d'enlever les commentaires Scheme (qui sont ils me semblent de la forme : ; ... \n) mais ce n'est sans doute pas prioritaire *)
(* RE-RE-EDIT NESTOR : il me semblait que toutes ces fonctions étaient déjà implantées dans split_string.ml sur le Github, elles n'allaient pas avec les automates ? *)
(* EDIT GUILLAUME : tu as sans doute raison, mais comme j'ai oublié ce qu'on avait déjà fait, je les ai réécrite "from scratch" *)
(* sépare la chaine u donnée en argument à chaque espace, transforme ces sous-chaînes en listes (via split) et en retourne la liste dans l'ordre de leur apparition dans u *)
let strip u =
   let l = split u in
   let res = ref [] in
   let rec aux l acc = match l with
         | [] -> res := acc :: (!res)
         | x :: r when x = ` ` or x = `\t` or x = `\n` ->
               begin res := acc :: (!res); aux r [] end
         | x :: r -> aux r (x :: acc)
   in aux l [];
   rev (map rev !res)
;;
(* Exemples : 
strip "fbdgd769\tzrr\nresf" ;;
strip "988**^ uhqa/\t" ;; 
*)

(* map_vect2 est similaire à map2 pour les listes :
map_vect2 f [|a1; ...; an|] [|b1; ...; bn|] is [|f a1 b1; ...; f an bn|]
Ne marche que si les deux tableaux en argument ont la même longueur *)
let map_vect2 f t u =
   if t = [||] then [||]
   else let n = vect_length t in
      let res = make_vect n (f t.(0) u.(0)) in
      for i = 1 to n - 1 do
         res.(i) <- f t.(i) u.(i)
      done;
      res
;;

(* Equivalent de exists pour les tableaux *)
let exists_vect p t = exists p (list_of_vect t)
;;

(* enleve l i retourne la liste l privé de ses i premiers éléments *)
let rec enleve l i = match l with
      | [] -> if i != 0 then failwith "trop d'elements a enlever" else l
      | _ :: r -> if i = 0 then l else enleve r (i - 1)
;;

(* aplatir prend une liste de liste en argument et retourne une liste obtenu par concaténation de ses éléments en conservant l'ordre *)
let aplatir liste = it_list (prefix @) [] liste
;;

(* sub l i j renvoie la sous_liste composée des éléments des indices i à j-1 *)
let rec sub l i j = match l with
      | [] -> if not (i = 0 && j = 0) then failwith "trop d'elements a enlever" else []
      | x :: r -> if i != 0 then sub r (i - 1) (j - 1)
        else if i = 0 && j = 0 then []  (* EDIT NESTOR : ici le "i = 0" ne sert a  rien *)
(* EDIT GUILLAUME : effectivement c'est une trace de l'ancien algo que j'ai oublié de modifié *)
        else x :: (sub r i (j - 1))
;;

(* équivalent de index pour les tableaux *)
(* EDIT NESTOR : dans index on met d'abord l'élément puis la liste, donc ce serait plus index_vect a t ... je chipote *)
(* EDIT NESTOR : chez moi index_vect ne fonctionne pas, il me sort une erreur vect_item, comme si tu essayais de chercher hors du tableau *)
(* EDIT NESTOR : j'ai trouvé l'erreur quand tu sors de la boucle, ton i peut valoir n et donc t.(!i) est hors du tableau *)
(* EDIT NESTOR : je ne comprends pas, il me sort cette erreur qu'avec un string_vect et pas avec d'autres types ... *)
(* EDIT GUILLAUME : j'ai eu quelques problèmes avec cette fonction... j'ai essayer de bidouiller tous mes indices et il me semble que cela marchait au final *)
let index_vect t a =
   let n = vect_length t in
   let i = ref 0 in
   while !i < n && t.(!i) != a do
      i := !i + 1
   done;
   if t.(!i) != a then failwith "l'element n'est pas dans le tableau"
   else !i
;;
(* EDIT NESTOR : je propose cette fonction là pour corriger le problème *)
(* EDIT GUILLAUME : niveau complexité c'est potentiellement un peu moins bien (on parcours obligatoirement tout le tableau) mais l'important c'est que ça marche *)
let index_vect2 t a =
  let n = vect_length t in
  let ind = ref 0 in
  for i = 1 to n
  do
    if t.(n-i) = a then ind := (n-i)
  done;
  if t.(!ind) = a then !ind
  else failwith "element absent du tableau" 
    ;;







(* DEFINITIONS DES AUTOMATES *)


(* foncton de transition de l'automate id, renvoie -1 si l'automate bloque *)
let trans_id q a =
   if (q = 0 && (mem a lettres)) or (q = 1 && (mem a sigma))
   then 1
   else - 1
;;

(* EDIT NESTOR : etats ne devrait pas etre egal à 2 ? mais comme tu disais plus haut, c'est pas bien grave si on utilise la longueur de finals *)
(* EDIT GUILLAUME : effectivement... *)
(* EDIT NESTOR : je ne comprends pourquoi si q=1 et a est dans sigma, on valide quand même ...
                 ca veut dire qu'on reconnait les noms de variables avec n'importe quoi dedans
                 genre as5+=sd56!! est un nom de variable ? 
                 je dirais qu'il faut juste accepter les chiffres et les lettres après 
                 donc avoir :
                   if (q = 0 && (mem a lettres)) || (q = 1 && ( (mem a lettres) || (mem a chiffres)))  *)
(* EDIT GUILLAUME : un identifiant en Scheme est juste un suite de charactères de l'alphabet que j'ai défini qui commence par une lettre... Donc oui : as5+=sd56!! est bien un identifiant légal ^^ *) 
(* automate reconnaissant les identifiants i.e. les noms de variables.
Ils sont de la forme lettres.(sigma)* *)
let id = {
      etats = 2;
      transition = trans_id;
      finals = [|false; true|] ;
      lexeme = "ID"
   }
;;


(* EDIT NESTOR : je ne suis pas sur mais je pense qu'on peut économiser un état.
                 On enlève le 6 et on met le 5 qui boucle sur lui meme si a est un chiffre.
                 Donc on enlève l'avant dernière ligne et on remplace celle d'avant par :
                     else if (q = 3 && a = `.`) || (q = 5 && ch) then 5
   je pense qu'on obtient la meme chose mais a verifier *)
(* RE-EDIT NESTOR : je pense qu'on peut meme le faire en 4 états : *)

let trans_nb2 q a =
  let ch = mem a chiffres in
  if q = 0 && a = `-` then 1
  else if q = 0 && ch then 2
  else if q = 1 && ch then 1
  else if q = 2 && ch then 2 (* EDIT GUILLAUME : il y a deux fois le même test... *)
  else if q = 2 && ch then 3 (* EDIT GUILLAUME : plutôt : if q = 3 non ? *) 
  else if (q=0 || q=1 || q=2) && a = `.` then 3
  else -1 (* EDIT GUILLAUME : j'ai aussi rajouté cette ligne *)
;;
(* EDIT GUILLAUME : il me semble que cet automate reconnaitrait "." comme étant un nombre... à voir *)
let nb2 = { etats = 4 ;
	    transition = trans_nb2 ;
	    finals = [| false; true; true; true |] ;
	    lexeme = "NB" }
;;
(* EDIT NESTOR : a voir si ca marche, a priori en dessinant l'automate ca marche *)

  
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
      finals = [|false; false; false; true; true; true; true|] ;
      lexeme = "NB"
   }
;;
(* Exemples : *)
reconnait nb2 "98635";;
reconnait nb2 "-546";;
reconnait nb2 "849.84635";;
reconnait nb2 "-484.446";;
reconnait nb2 "8476.";;
reconnait nb2 "-8448.";;
reconnait nb2 ".8965";;
reconnait nb2 "-.8845";;
reconnait nb2 ".";;
reconnait nb2 "-8845f846";; 



(* fonction de transition de l'automate associé au langage {u} *)
let trans_mot u q a =
let n = string_length u in 
   if q != -1 && q != (n) && u.[q] = a then q+1
   else - 1
;;

(* construit l'automate reconnaissant uniquement le mot u *)
let mk_auto_mot mot =
   let n = string_length mot in
   let t = make_vect (n + 1) false in
   t.(n) <- true;
   {etats = n + 1;
      transition = trans_mot mot;
      finals = t;
      lexeme = "KEYWD"
   }
;;

(* fonction de transition de op *)
let trans_op q a =
   if q = 0 && (mem a operations) then 1
   else - 1
;;

(* automate reconnaissant les 4 opérations élémentaires *)
let op = {
      etats = 2;
      transition = trans_op;
      finals = [|false; true|];
      lexeme = "OP"
   }
;;


(* crée l' "automate produit" global A_vect : les automates sont rangés par ordre de priorité *)
let longueur_keywd = vect_length keyword ;; 
let A_vect = make_vect (longueur_keywd+3) id 
;;

let n = vect_length keyword in 
let i = ref 0 in
do_vect (fun x -> A_vect.(!i) <- mk_auto_mot x ; i := !i+1 ) keyword ;
A_vect.(n) <- op ;
A_vect.(n+1) <- id ;
A_vect.(n+2) <- nb 
;;










(* FONCTIONS PRINCIPALES *)


(* trans_all prend en argument un tableau d'automates et un tableau d'états et retourne le tableau des états dans lequel chacun des automates se trouve après la transition à partir de l'état correspondant par a *)
let trans_all A_vect q_vect a =
   map_vect2 (fun A q -> if q != -1 then A.transition q a else -1) A_vect q_vect
;;



(* fonction magique :
u est un mot "découpé" (par split par exemple) donc une liste de charactères
anallex_lst retourne la liste des couples (reconnait,i) où 
- reconnait est un tableau indiquant pour chaque automate de A_vect si il reconnait ce lexème ;
- i est la longueur du lexème reconnu
Note : les lexèmes reconnus sont les plus longs qu'il était possible de reconnaitre *)
let anallex_mot u =

   let l = ref u in
   let n = vect_length A_vect in
   let q_vect = make_vect n 0 in (* q_vect.(i) est l'état de l'automate numéro i *)
   let res = ref [] in
   (* lex effectue la lecture du charactère suivant:
   - i est le nombre de charactère lu jusqu'à présent, utile car on veut connaitre la longueur du  mot reconnu
- s est le mot que l'on est en train de lire 
 - flag vaut true si l'étape précédente était la dernière possible : il permet de sortir *)
   let rec lex q_vect i s flag =
      if flag then q_vect, (i - 1)
      else match s with
            | [] -> begin print_vect q_vect;
                     print_int i;
                     lex q_vect (i + 1) s true
                  end
            | x :: r -> let suiv = trans_all A_vect q_vect x in
                  begin (* debug : print_char x;
                     print_vect suiv;
                      print_int i; *)
                     if exists_vect (fun x -> x != - 1) suiv then lex suiv (i + 1) r false
                     else lex q_vect (i + 1) r true end
   in
   (* on reconnait le premier lexème de la chaine puis on l'enlève et on recommence jusqu'à ce que la liste soit vide *)
   while !l != [] do
      let q_vect', i = lex q_vect 0 !l false in
      res := (q_vect', i) :: (!res);
      l := enleve !l i
   done;
   let map2_first f t1 t2 =
      (map_vect2 f t1 (fst t2)), (snd t2)
   in
   map (map2_first (fun A q -> (q != - 1) && A.finals.(q)) A_vect) (!res)
;;
(* anallex découpe la chaine donnée en argument en mot avec strip puis retourne la liste des application de anallex_mot sur chaque mot obtenus *)
let anallex chaine =
   let liste = strip chaine in
   map (fun l -> rev (anallex_mot l)) liste
;;

(* Traitement a pour but de transformer le résultat de anallex à l'aide de la chaine initiale pour obtenir une liste de lexèmes. 
Pour chaque lexèmes on a sa longueur ainsi qu'un tableau des automates le reconnaissant, on prend le premier de celui-ci considéré comme prioritaire puis on associe le lexème de l'automate à la chaine correspondante (que l'on peut retrouver grâce à sa longueur.
On fait alors une liste de tous ces couples (lexèmes , chaîne) *)
let traitement liste chaine =
   let l = aplatir liste in
   let u = aplatir chaine in
   let rec aux l j = match l with
         | [] -> []
         | x :: r -> begin let vect_q, i = x in
                  let k = index_vect vect_q true in
                  let lexeme = A_vect.(k).lexeme in
                  let s = create_string i in
                  let ss_liste = sub u j (j + i) in
                  let c = ref 0 in
                  do_list (fun x -> s.[!c] <- x; c := !c + 1) ss_liste;
                  (lexeme, s) :: (aux r (j + i))
               end
   in aux l 0
;;

(* FONCTION FINALE : Réalise l'analyse lexicale de la chaîne de charactères donnée en argument *)
let analyse_lexicale chaine =
   traitement (anallex chaine) (strip chaine)
;;
 
let chaîne =  "(define ab (+ 5 3))" ;;
analyse_lexicale chaîne ;;








