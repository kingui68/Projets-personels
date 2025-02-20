(* Principes :
On souhaite identifier les lex�mes d'une chaine de charact�res.
Tout ces lex�mes peuvent �tre d�crits par des expressions rationnelles et sont donc reconnaissables par des automates d�terministes.
Dans un premier temps un construit un arbre pour chacune de ces expressions rationnelles.
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
let chiffres = [`1`; `2`; `3`; `4`; `5`; `6`; `7`; `8`; `9`]
;;
let char_spe = [`?`; `!`; `.`; `+`; `-`; `*`; `/`; `<`; `=`; `>`; `;`; `$`; `%`; `^`; `&`; `_`; `~`; `@`] 
;;
(* sigma est l'alphabet compos� des charact�res accept�s par scheme *)
let sigma = lettres @ chiffres @ char_spe
;;
(* Tableau des mots_clefs reconnus *)
let keyword = [|"define";"(";")";"set";"if";"then";"else"|]
;;
let operations = [`+`; `-`; `*`; `/`]
;;
(* type d'automates finis d�terministe
Note : le nombre d'�tat n'est pas tr�s utile, on peut l'avoir avec la longueur de finals... mais je pr�f�re faire ainsi *)
type automate = {
      etats: int; (* etats repr�sentent le nombre d'�tats, ils sont repr�sent�s par les entiers [0,...,etats -1] *)
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
let prefixe s t = compare_strings s t = - 2
;;

(* split u retourne la liste des charact�res de u apparaissant dans l'ordre *)
let split u =
   let n = string_length u in
   let l = ref [] in
   for i = 0 to n - 1 do
      l := u.[i] :: (!l)
   done;
   rev !l
;;

(* reconnait A u retourne true si l'automate A reconnait le mot u (sous forme de cha�ne de charact�res)
Utile uniquement pour la programmation *)
let reconnait A u =
   let delta = A.transition in
   let l = split u in
   let rec aux l q = match l with
         | [] -> not (q = - 1) && begin try A.finals.(q) with
                  | nth_char -> false end
         | x :: r -> let q' = delta q x in
               not (q' = - 1) && aux r q'
   in aux l 0
;;

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
      | [] -> if i != 0 then failwith "trop d'elements a enleve" else l
      | _ :: r -> if i = 0 then l else enleve r (i - 1)
;;

(* aplatir prend une liste de liste en argument et retourne une liste obtenu par concat�nation de ses �l�ments en conservant l'ordre *)
let aplatir liste = it_list (prefix @) [] liste
;;

(* suj l i j renvoie la sous_liste compos�e des �l�ments des indices i � j-1 *)
let rec sub l i j = match l with
      | [] -> if not (i = 0 && j = 0) then failwith "trop d'elements a enlever" else []
      | x :: r -> if i != 0 then sub r (i - 1) (j - 1)
            else if i = 0 && j = 0 then []
            else x :: (sub r i (j - 1))
;;

(* �quivalent de index pour les tableaux *)
let index_vect t a =
   let n = vect_length t in
   let i = ref 0 in
   while !i < n && t.(!i) != a do
      i := !i + 1
   done;
   if t.(!i) != a then failwith "l'element n'est pas dans le tableau"
   else !i
;;











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
      finals = [|false; true|] ;
      lexeme = "ID"
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
(* Exemples :
reconnait nb "98635";;
reconnait nb "-546";;
reconnait nb "849.84635";;
reconnait nb "-484.446";;
reconnait nb "8476.";;
reconnait nb "-8448.";;
reconnait nb ".8965";;
reconnait nb "-.8845";;
reconnait nb ".";;
reconnait nb "-8845f846";; *)



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
(* Exemple : 
let cha�ne =  "(define ab (+ 5 3))" ;;
analyse_lexicale cha�ne ;;
*)








