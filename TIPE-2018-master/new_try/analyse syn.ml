type lexeme =
   NB of string (* nombres *)
   | ID  of string  (* identifiants, variables *)
   | OP of string  (* opérateurs usuels : +, -, *, / *)
   | DEF          (* define *)
   | LPAR         (* ( *)
   | RPAR         (* ) *)
;;
(* Premier essai : grammaire ambigu il me semble... plus qu'à recommencer 
(* Alphabet de la grammaire *)
type grammaire =
     ListExpr (* Axiome *)
   | EOF (* End of file : fin du programme *)
   | Expr
   | Eps (* Epsilon : mot vide *)
   | BlocFct
   | Valeur
   | ResteVal
   | Arg
   | NB (* nombres *)
   | ID   (* identifiants, variables *)
   | OP   (* opérateurs usuels : +, -, *, / *)
   | DEF          (* define *)
   | LPAR         (* ( *)
   | RPAR         (* ) *)
   | Erreur (* artitfice pour remplir la table d'analyse avec un seul type *)
;;
let G = [| ListExpr; Expr; Valeur; ResteVal; BlocFct; Arg; NB; ID; OP; DEF; LPAR; RPAR; EOF; Eps|]
;;
(* Tableaux des non-terminaux *)
let non_term = [|ListExpr; Expr; Valeur; ResteVal; BlocFct; Arg|]
;;
(* Tableaux des terminaux *)
let term = [|NB; ID; OP; DEF; LPAR; RPAR; EOF; Eps|]
;;
(* Règles de production *)
let production g = match g with
      | ListExpr -> [|[|Expr; ListExpr|]; [|Eps|]|]
      | Expr -> [|[|DEF; BlocFct; Valeur|]; [|DEF; ID; Valeur|]; [|Valeur|]|]
      | Valeur -> [|[|NB|]; [|LPAR; OP; Valeur; ResteVal|]; [|ID|]; [|BlocFct|]|]
      | ResteVal -> [|[|Valeur|]; [|Valeur; ResteVal|]|]
      | BlocFct -> [|[|LPAR; ID; Arg; RPAR|]|]
      | Arg -> [|[|ID; Arg|]; [|Eps|]|]
      | x -> [|[|x|]|]
;;

(* fonction attribuant le tableau des premiers terminaux aux différents symboles de la grammaire.
Note : calculé à la main... *)
let Prem_sym X = match X with
  |ListExpr -> [|Eps; LPAR; NB; ID|]
  |Expr -> [|LPAR; NB; ID|]
  |Valeur -> [|NB; LPAR; ID|]
  |ResteVal -> [|NB; LPAR; ID|]
  |BlocFct -> [|LPAR|]
  |Arg -> [|ID; Eps|]
  |Eps -> [||]
  |x -> [|x|]
;;

(* fonction attribuant le tableau des premiers terminaux pouvant suivre les différents symboles de la grammaire.
Note : calculé à la main... *)

let Suiv X = match X with
      | ListExpr -> [|LPAR; NB; ID; EOF|]
      | Expr -> [|LPAR; NB; ID; EOF|]
      | Valeur -> [|NB; LPAR; ID; EOF|]
      | ResteVal -> [|NB; LPAR; ID; RPAR|]
      | BlocFct -> [|NB; LPAR; ID; EOF|]
      | Arg -> [|ID; RPAR|]
      | Eps -> [||]
      | _ -> failwith "l'argument n'est pas un non_terminal"
;;

*)

(* FONCTIONS AUXILIAIRES *)

let vect_union t1 t2 = vect_of_list (union (list_of_vect t1) (list_of_vect t2))
;;
let vect_mem a t = mem a (list_of_vect t) ;;
let depush pile =
   let x = stack__pop pile in ()
;;

(* Deuxième version proposée par Nestor
Même fonction que dans analyse lex.ml *)
let index_vect2 t a =
   let n = vect_length t in
   let ind = ref 0 in
   for i = 1 to n
   do
      if t.(n - i) = a then ind := (n - i)
   done;
   if t.(!ind) = a then !ind
   else failwith "element absent du tableau"
;;



type grammaire =
   ListExpr
   | Expr
   | Instr
   | Var
   | Arg
   | Arith
   | ResteArg
   | App
   | EOF
   | Eps
   | LPAR
   | RPAR
   | DEF
   | ID
   | NB
   | OP
   | Erreur
;;
(* Tableaux des terminaux *)
let term = [|EOF; Eps; LPAR; RPAR; DEF; ID; NB; OP|]
;;
(* Tableaux des non-terminaux *)
let non_term = [|ListExpr; Expr; Instr; Var; Arg; Arith; ResteArg; App|]
;;

(* Règles de production *)
let production g = match g with
      | ListExpr -> [|[|Expr; ListExpr|]; [|Eps|]|]
      | Expr -> [|[|NB|]; [|ID|]; [|LPAR; Instr; RPAR|]|]
      | Instr -> [|[|DEF; Var; Arith|]; [|App|]|]
      | Var -> [|[|ID|]; [|LPAR; ID; Arg; RPAR|]|]
      | Arg -> [|[|Eps|]; [|ID; Arg|]|]
      | Arith -> [|[|NB|]; [|ID|]; [|LPAR; App; RPAR|]|]
      | ResteArg -> [|[|Eps|]; [|Arith; ResteArg|]|]
      | App -> [|[|OP;Arith;Arith;ResteArg|]; [|ID; ResteArg|]|]
      | x -> [|[|x|]|]
;;

(* fonction attribuant le tableau des premiers terminaux aux différents symboles de la grammaire.
Note : calculé à la main... *)
let Prem_sym X = match X with
      | ListExpr -> [|NB; ID; LPAR; Eps|]
      | Expr -> [|NB; ID; LPAR|]
      | Instr -> [|DEF; OP; ID|]
      | Var -> [|ID; LPAR|]
      | Arg -> [|Eps; ID|]
      | Arith -> [|NB; ID; LPAR|]
      | ResteArg -> [|Eps; NB; ID; LPAR|]
      | App -> [|OP; ID|]
      | Eps -> [|Eps|]
      | x -> [|x|]
;;

(* fonction attribuant le tableau des premiers terminaux pouvant suivre les différents symboles de la grammaire.
Note : calculé à la main... *)

let Suiv X = match X with
      | ListExpr -> [|EOF|]
      | Expr -> [||]
      | Instr -> [||]
      | Var -> [||]
      | Arg -> [|RPAR|]
      | Arith -> [||]
      | ResteArg -> [|RPAR|]
      | App -> [||]
      | Eps -> [||]
      | _ -> failwith "l'argument n'est pas un non_terminal"
;;

let Prem_mot alpha =
   let n = vect_length alpha in
   let rec aux beta i =
      let prem = Prem_sym beta.(i) in
      if mem Eps (list_of_vect prem) && i < (n - 1) then vect_union prem (aux beta (i + 1))
      else prem
   in aux alpha 0
;;

let print_gram g = match g with
      | ListExpr -> print_string "ListExpr"
      | Expr -> print_string "Expr"
      | Instr -> print_string "Instr"
      | Var -> print_string "Var"
      | Arg -> print_string "Arg"
      | Arith -> print_string "Arith"
      | ResteArg -> print_string "ResteArg"
      | App -> print_string " App->"
      | EOF -> print_string "EOF->"
      | Eps -> print_string "Eps->"
      | LPAR -> print_string "LPAR->"
      | RPAR -> print_string "RPAR"
      | DEF -> print_string "DEF"
      | ID -> print_string "ID"
      | NB -> print_string "NB"
      | OP -> print_string "OP"
      | Erreur -> print_string " Erreur"
            ; format__print_space ()
;;

 






(* Construction de la table d'analyse par l'algorithme décrit sur wikipédia :
https://fr.wikipedia.org/wiki/Analyse_LL *)
let nb_non_term = vect_length non_term;;
let nb_term = vect_length term ;;
let table_analyse = make_matrix nb_non_term nb_term [Erreur] ;;

for i = 0 to nb_non_term - 1 do
   let X = non_term.(i) in (* X est le i-ème non-terminal de non_term *)
   let prod = production X in (* prod est le tableau des alpha tels que X->alpha *)
   for k = 0 to vect_length prod - 1 do
      let alpha = prod.(k) in (* alpha est le mot correspondant au k-ième choix de X *)
      let prem = Prem_mot alpha in (* prem est le tableau Prem(alpha) *)
      for l = 0 to vect_length prem - 1 do
         let a = prem.(l) in (* a est le l-ième terminal de Prem(alpha) *)
         if a = Eps then
            let suiv = Suiv X in
            for m = 0 to vect_length suiv - 1 do
               let b = suiv.(m) in (* b est le m-ième terminal de Suiv(X) *)
               let j = index_vect2 term b in
               table_analyse.(i).(j) <- [Eps]
            done
         else let j = index_vect2 term a in
            table_analyse.(i).(j) <- (list_of_vect alpha)
      done
   done
done
;;
table_analyse ;;

let est_vide pile = stack__length pile = 0 ;;




(* version 1 : retourne simplement true si la syntaxe est correcte, false ou une erreur sinon

let rec choix l pile = match l with
         | [] -> failwith "choix vide"
         | [x] when x = Eps -> ()
         | [x] -> stack__push x pile
         | x :: r -> choix r pile; stack__push x pile
  ;;


   let rec fini pile =
      let top = stack__pop pile in
      top = EOF || begin
         let i = index_vect2 non_term top in
         let j = index_vect2 term EOF in
         table_analyse.(i).(j) = [Eps] && (fini pile) end
   ;;

let analyse_syntaxique lexbuf =
   let pile = stack__new () in
   stack__push EOF pile;
   stack__push ListExpr pile;
   let rec step lexbuf = match lexbuf with
         | [] -> est_vide pile
         | [x] when (fst x) = EOF -> fini pile
         | lexeme :: r -> let top = stack__pop pile in
         print_gram top ;
               if vect_mem top non_term then
                  let i = index_vect2 non_term top in
                  let j = index_vect2 term (fst lexeme) in
                  print_gram (fst lexeme);
                  let l = table_analyse.(i).(j) in
                  if l = [Erreur] then failwith "Erreur syntaxique2"
                  else begin choix l pile; step lexbuf end
               else if top = (fst lexeme) then step r
               else failwith "Erreur syntaxique"
   in step lexbuf
;;
*)


(*définition d’un arbre hétérogène à l’aide de tableaux *)
type  arbre =
| Feuille of grammaire * string
| Noeud of grammaire*foret
and foret =
Arbres of arbre vect
;;


let rec iter_arbre f arbre = match arbre with
      | Feuille(g) -> f (Feuille(g))
      | Noeud(g, foret) ->
            begin f (Noeud(g, foret));
               match foret with
                  | Arbres (t) -> do_vect (iter_arbre f) t
            end
;;
let print_noeud noeud = match noeud with
      | Feuille (g, s) -> begin print_string "F"; print_gram g; print_string ","; print_string s; print_string "," end
      | Noeud (g, foret) -> begin print_string "N"; print_string " "; print_gram g; print_string " " end
;; 
let print_arbre arbre = iter_arbre print_noeud arbre ;;








let analyse_syntaxique lexbuf =
   let pile_gram = stack__new () in
   let pile_arbre = stack__new () in

   stack__push EOF pile_gram;
   stack__push ListExpr pile_gram;

   let default = Feuille (Erreur, "") in
   let a = [|default|] in
   let arbre = Noeud (EOF, Arbres (a)) in
   stack__push (a, 0, 1) pile_arbre;

   let rec choix l pile = match l with
         | [] -> failwith "choix vide"
         | [x] -> stack__push x pile
         | x :: r -> choix r pile; stack__push x pile
   in

   let rec step lexbuf = match lexbuf with
         | [] -> if est_vide pile_gram then arbre
               else failwith "Erreur syntaxique1"

         | lexeme :: r -> let top = stack__pop pile_gram in

               if vect_mem top non_term then
                  let i = index_vect2 non_term top in
                  let j = index_vect2 term (fst lexeme) in
                  let l = table_analyse.(i).(j) in
                  if l = [Erreur] then failwith "Erreur syntaxique2"
                  else begin
                        choix l pile_gram;
                        let fils, c, n = stack__pop pile_arbre in
                        let n' = list_length l in
                        let t = make_vect n' default in
                        fils.(c) <- Noeud (top, Arbres (t));
                        if c + 1 < n then begin
                              stack__push (fils, c + 1, n) pile_arbre;
                              stack__push (t, 0, n') pile_arbre;
                           end
                        else stack__push (t, 0, n') pile_arbre;
                        step lexbuf
                     end

               else if top = Eps then begin
                     let fils, c, n = stack__pop pile_arbre in
                     fils.(c) <- Feuille(Eps,"");
                     if c + 1 < n then stack__push (fils, c + 1, n) pile_arbre;
                     step lexbuf end
                     
               else if top = EOF && (fst lexeme) = EOF then arbre

               else if top = (fst lexeme) then
                  let fils, c, n = stack__pop pile_arbre in
                  fils.(c) <- Feuille (lexeme);
                  if c + 1 < n then stack__push (fils, c + 1, n) pile_arbre;
                  step r




               else failwith "Erreur syntaxique3"
   in step lexbuf
;;


(* Exemple *)
let lexbuf = [LPAR, "("; DEF, "define"; ID, "ab"; LPAR, "("; OP, "+"; NB, "5"; NB, "3"; RPAR, ")"; RPAR, ")"; EOF, "$"];;
analyse_syntaxique lexbuf ;;


 



