

(* Principes
On souhaite identifier les lex�mes d'une chaine de charact�res.
Tout ces lex�mes peuvent �tre d�crits par des expressions rationnelles et sont donc reconnaissables par des automates d�terministes.
Dans un premier temps un construit un arbre pour chacune de ces expressions rationnelles.C
Ensuite on r�alise l'automate produit (non d�terministe). Plus pr�cis�ment on simule la construction d'un automate produit pour �viter de construire tous les �tats. Dans les faits, on fait fonctionner tous les automates en parall�les sur la cha�ne de charact�res et on s'arr�te juste avant que tous les automates ne bloquent. 
On a alors la liste des automates qui reconnaissent le plus long lex�me, il suffit alors de d�finir un ordre de priorit� pour choisir l'action.
Ici l'ordre est d�fini par l'ordre d'apparition des automates dans l'automate produit.
(ex : let peut �tre consid�r� comme le mot cl� let ou l'identifiant let)
 *)  

(* La fonction finale est analyse_lexicale qui prend un mot sous forme de cha�ne de charact�res et retourne la liste des lex�mes *)

(* Pour que cette analyse lexicale reconnaisse de nouveaux mots cl�s, et avec la bonne priorit� il suffit de les rajouter au bon endroit dans le tableau keyword *)






(* DEFINITIONS *)

let lettres = [`a`; `b`; `c`; `d`; `e`; `f`; `g`; `h`; `i`; `j`; `k`; `l`; `m`; `n`; `o`; `p`; `q`; `r`; `s`; `t`; `u`; `v`; `w`; `x`; `y`; `z`; `A`; `B`; `C`; `D`; `E`; `F`; `G`; `H`; `I`; `J`; `K`; `L`; `M`; `N`; `O`; `P`; `Q`; `R`; `S`; `T`; `U`; `V`; `W`; `X`; `Y`; `Z`]
;;
let chiffres = [`1`; `2`; `3`; `4`; `5`; `6`; `7`; `8`; `9`; `0`] (* EDIT NESTOR : il manquait le zero, je l'ai rajout� *)
;;
let char_spe = [`?`; `!`; `.`; `+`; `-`; `*`; `/`; `<`; `=`; `>`; `;`; `$`; `%`; `^`; `&`; `_`; `~`; `@`] (* EDIT NESTOR : je sais pas si on doit mettre ou pas la virgule *)
(* EDIT GUILLAUME : Cette liste est en r�alit� la liste des charact�res qui peuvent appara�tre dans un nom de variable. Je l'ai r�cup�r� sur wikip�dia... La virgule par exemple ne fait donc pas partie de cette liste. *)
;;
(* sigma est l'alphabet compos� des charact�res accept�s par scheme *)
let sigma = lettres @ chiffres @ char_spe
;;
(* Tableau des mots_clefs reconnus *)
let keyword = [|"define";"(";")";"set";"if";"then";"else"|]  (* EDIT NESTOR : je ne comprends pas pourquoi on doit utiliser un vect au lieu d'un list ici, ca doit
                                                                etre pour la suite que je n'ai pas encore lue ^^ *)
(* EDIT GUILLAUME : leur nombre n'est pas tr�s grand donc la place en m�moire n'est pas tr�s probl�matique... Les fonctions �taient juste plus simple � �crire avec un tableau ^^ *)
	       
;;
let operations = [`+`; `-`; `*`; `/`]
;;
(* type d'automates finis d�terministes
Note : le nombre d'�tats n'est pas tr�s utile, on peut l'avoir avec la longueur de finals... mais je pr�f�re faire ainsi *)
type automate = {
      etats: int; (* etats repr�sente le nombre d'�tats, ils sont repr�sent�s par les entiers [0,...,etats -1] *)
      transition: int -> char -> int;
      finals: bool vect ;(* finals.[i] contient true si i est un �tat final, false sinon *)
      lexeme : string (* repr�sente l'"action" : permet d'associer le nom du lex�me *)
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
note : cette fonction s'av�re inutile *)
let prefixe s t = compare_strings s t = - 2  (* EDIT NESTOR : compare_strings est une fonction de base Caml ? *)
;;                                           (* RE-EDIT NESTOR : c'est bon guillaume m'a expliqu� ! *)
                                             (* RE-RE-EDIT NESTOR : et puis de toute facon on s'en sert pas bezef *)


(* split u retourne la liste des charact�res de u apparaissant dans l'ordre *)
let split u =
   let n = string_length u in
   let l = ref [] in
   for i = 0 to n - 1 do
      l := u.[i] :: (!l)
   done;
   rev !l
;;


(* EDIT NESTOR : *)
(* split_rec u fait la m�me chose que split u mais en r�cursif, je sais pas si c'est mieux ... *)
(* EDIT GUILLAUME : Fut�, mais honn�tement je ne pense pas que ce soit tr�s important *) 
let split_rec u =
  let n = string_length u in
  let rec split_aux u i =
    if i = n then []
    else u.[i] :: (split_aux u (i+1))
  in split_aux u 0
;;

(* EDIT NESTOR : J'ai juste mis un peu en forme la fonction parce que j'arrivais pas � lire � cause de mon �cran et de mes yeux pourris xD *)
(* reconnait A u retourne true si l'automate A reconnait le mot u (sous forme de cha�ne de charact�res)
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

(* EDIT NESTOR : si on met deux espaces ou deux s�parations � la suite, ca cr�e des sous-chaines vides ... et du coup des listes de caract�res vides, c'est grave ? *)
(* EDIT GUILLAUME : en effet cela cr�e des listes vides mais cela n'a aucune importance... (notamment pour la complexit�) *)
(* RE-EDIT NESTOR : ne vaudrait-il pas mieux utiliser une liste de caract�res s�parant ? genre pour inclure le point-virgule ou la virgule ? *)
(* Ceux qu j'ai mis sont les seules autoris�es en Scheme normalement... pas de virgule ou de points-virgules comme s�parateurs en Scheme. Il est cependant possible (mais vraisemblablement inutile) soit d'enlever les listes vides apr�s, soit de construire un automate reconnaissant les concat�nations de s�parateurs  (genre "\t  \n") *)
(* EDIT GUILLAUME : J'ai n�anmoins omis de reconnaitre et d'enlever les commentaires Scheme (qui sont ils me semblent de la forme : ; ... \n) mais ce n'est sans doute pas prioritaire *)
(* RE-RE-EDIT NESTOR : il me semblait que toutes ces fonctions �taient d�j� implant�es dans split_string.ml sur le Github, elles n'allaient pas avec les automates ? *)
(* EDIT GUILLAUME : tu as sans doute raison, mais comme j'ai oubli� ce qu'on avait d�j� fait, je les ai r��crite "from scratch" *)
(* s�pare la chaine u donn�e en argument � chaque espace, transforme ces sous-cha�nes en listes (via split) et en retourne la liste dans l'ordre de leur apparition dans u *)
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

(* map_vect2 est similaire � map2 pour les listes :
map_vect2 f [|a1; ...; an|] [|b1; ...; bn|] is [|f a1 b1; ...; f an bn|]
Ne marche que si les deux tableaux en argument ont la m�me longueur *)
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

(* enleve l i retourne la liste l priv� de ses i premiers �l�ments *)
let rec enleve l i = match l with
      | [] -> if i != 0 then failwith "trop d'elements a enlever" else l
      | _ :: r -> if i = 0 then l else enleve r (i - 1)
;;

(* aplatir prend une liste de liste en argument et retourne une liste obtenu par concat�nation de ses �l�ments en conservant l'ordre *)
let aplatir liste = it_list (prefix @) [] liste
;;

(* sub l i j renvoie la sous_liste compos�e des �l�ments des indices i � j-1 *)
let rec sub l i j = match l with
      | [] -> if not (i = 0 && j = 0) then failwith "trop d'elements a enlever" else []
      | x :: r -> if i != 0 then sub r (i - 1) (j - 1)
        else if i = 0 && j = 0 then []  (* EDIT NESTOR : ici le "i = 0" ne sert a  rien *)
(* EDIT GUILLAUME : effectivement c'est une trace de l'ancien algo que j'ai oubli� de modifi� *)
        else x :: (sub r i (j - 1))
;;

(* �quivalent de index pour les tableaux *)
(* EDIT NESTOR : dans index on met d'abord l'�l�ment puis la liste, donc ce serait plus index_vect a t ... je chipote *)
(* EDIT NESTOR : chez moi index_vect ne fonctionne pas, il me sort une erreur vect_item, comme si tu essayais de chercher hors du tableau *)
(* EDIT NESTOR : j'ai trouv� l'erreur quand tu sors de la boucle, ton i peut valoir n et donc t.(!i) est hors du tableau *)
(* EDIT NESTOR : je ne comprends pas, il me sort cette erreur qu'avec un string_vect et pas avec d'autres types ... *)
(* EDIT GUILLAUME : j'ai eu quelques probl�mes avec cette fonction... j'ai essayer de bidouiller tous mes indices et il me semble que cela marchait au final *)
let index_vect t a =
   let n = vect_length t in
   let i = ref 0 in
   while !i < n && t.(!i) != a do
      i := !i + 1
   done;
   if t.(!i) != a then failwith "l'element n'est pas dans le tableau"
   else !i
;;
(* EDIT NESTOR : je propose cette fonction l� pour corriger le probl�me *)
(* EDIT GUILLAUME : niveau complexit� c'est potentiellement un peu moins bien (on parcours obligatoirement tout le tableau) mais l'important c'est que �a marche *)
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

(* EDIT NESTOR : etats ne devrait pas etre egal � 2 ? mais comme tu disais plus haut, c'est pas bien grave si on utilise la longueur de finals *)
(* EDIT GUILLAUME : effectivement... *)
(* EDIT NESTOR : je ne comprends pourquoi si q=1 et a est dans sigma, on valide quand m�me ...
                 ca veut dire qu'on reconnait les noms de variables avec n'importe quoi dedans
                 genre as5+=sd56!! est un nom de variable ? 
                 je dirais qu'il faut juste accepter les chiffres et les lettres apr�s 
                 donc avoir :
                   if (q = 0 && (mem a lettres)) || (q = 1 && ( (mem a lettres) || (mem a chiffres)))  *)
(* EDIT GUILLAUME : un identifiant en Scheme est juste un suite de charact�res de l'alphabet que j'ai d�fini qui commence par une lettre... Donc oui : as5+=sd56!! est bien un identifiant l�gal ^^ *) 
(* automate reconnaissant les identifiants i.e. les noms de variables.
Ils sont de la forme lettres.(sigma)* *)
let id = {
      etats = 2;
      transition = trans_id;
      finals = [|false; true|] ;
      lexeme = "ID"
   }
;;


(* EDIT NESTOR : je ne suis pas sur mais je pense qu'on peut �conomiser un �tat.
                 On enl�ve le 6 et on met le 5 qui boucle sur lui meme si a est un chiffre.
                 Donc on enl�ve l'avant derni�re ligne et on remplace celle d'avant par :
                     else if (q = 3 && a = `.`) || (q = 5 && ch) then 5
   je pense qu'on obtient la meme chose mais a verifier *)
(* RE-EDIT NESTOR : je pense qu'on peut meme le faire en 4 �tats : *)

let trans_nb2 q a =
  let ch = mem a chiffres in
  if q = 0 && a = `-` then 1
  else if q = 0 && ch then 2
  else if q = 1 && ch then 1
  else if q = 2 && ch then 2 (* EDIT GUILLAUME : il y a deux fois le m�me test... *)
  else if q = 2 && ch then 3 (* EDIT GUILLAUME : plut�t : if q = 3 non ? *) 
  else if (q=0 || q=1 || q=2) && a = `.` then 3
  else -1 (* EDIT GUILLAUME : j'ai aussi rajout� cette ligne *)
;;
(* EDIT GUILLAUME : il me semble que cet automate reconnaitrait "." comme �tant un nombre... � voir *)
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
Plus pr�cis�ment un nombre est de la forme :
 (- + eps).chiffres.chiffres*.(`.` + eps).chiffres*
 ou (- + eps).`.`chiffres.chiffres*
 avec eps le mot vide et . la concat�nation.
 Autrement dit les nombres peuvent etre pr�c�d� d'un - et � virgule (not� . ).
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



(* fonction de transition de l'automate associ� au langage {u} *)
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

(* automate reconnaissant les 4 op�rations �l�mentaires *)
let op = {
      etats = 2;
      transition = trans_op;
      finals = [|false; true|];
      lexeme = "OP"
   }
;;


(* cr�e l' "automate produit" global A_vect : les automates sont rang�s par ordre de priorit� *)
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


(* trans_all prend en argument un tableau d'automates et un tableau d'�tats et retourne le tableau des �tats dans lequel chacun des automates se trouve apr�s la transition � partir de l'�tat correspondant par a *)
let trans_all A_vect q_vect a =
   map_vect2 (fun A q -> if q != -1 then A.transition q a else -1) A_vect q_vect
;;



(* fonction magique :
u est un mot "d�coup�" (par split par exemple) donc une liste de charact�res
anallex_lst retourne la liste des couples (reconnait,i) o� 
- reconnait est un tableau indiquant pour chaque automate de A_vect si il reconnait ce lex�me ;
- i est la longueur du lex�me reconnu
Note : les lex�mes reconnus sont les plus longs qu'il �tait possible de reconnaitre *)
let anallex_mot u =

   let l = ref u in
   let n = vect_length A_vect in
   let q_vect = make_vect n 0 in (* q_vect.(i) est l'�tat de l'automate num�ro i *)
   let res = ref [] in
   (* lex effectue la lecture du charact�re suivant:
   - i est le nombre de charact�re lu jusqu'� pr�sent, utile car on veut connaitre la longueur du  mot reconnu
- s est le mot que l'on est en train de lire 
 - flag vaut true si l'�tape pr�c�dente �tait la derni�re possible : il permet de sortir *)
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
   (* on reconnait le premier lex�me de la chaine puis on l'enl�ve et on recommence jusqu'� ce que la liste soit vide *)
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
(* anallex d�coupe la chaine donn�e en argument en mot avec strip puis retourne la liste des application de anallex_mot sur chaque mot obtenus *)
let anallex chaine =
   let liste = strip chaine in
   map (fun l -> rev (anallex_mot l)) liste
;;

(* Traitement a pour but de transformer le r�sultat de anallex � l'aide de la chaine initiale pour obtenir une liste de lex�mes. 
Pour chaque lex�mes on a sa longueur ainsi qu'un tableau des automates le reconnaissant, on prend le premier de celui-ci consid�r� comme prioritaire puis on associe le lex�me de l'automate � la chaine correspondante (que l'on peut retrouver gr�ce � sa longueur.
On fait alors une liste de tous ces couples (lex�mes , cha�ne) *)
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

(* FONCTION FINALE : R�alise l'analyse lexicale de la cha�ne de charact�res donn�e en argument *)
let analyse_lexicale chaine =
   traitement (anallex chaine) (strip chaine)
;;
 
let cha�ne =  "(define ab (+ 5 3))" ;;
analyse_lexicale cha�ne ;;








