type lexeme =
   NB of string (* nombres *)
   | ID  of string  (* identifiants, variables *)
   | OP of string  (* opérateurs usuels : +, -, *, / *)
   | DEF          (* define *)
   | LPAR         (* ( *)
   | RPAR         (* ) *)
;;

(* Alphabet de la grammaire *)
type grammaire =
   S (* Axiome *)
   | ListExpr
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
;;
let G = [|S; ListExpr; Expr; Valeur; ResteVal; BlocFct; Arg; NB; ID; OP; DEF; LPAR; RPAR; EOF; Eps|]
;;
(* Règles de production *)
let production g = match g with
      | S -> [[ListExpr; EOF]]
      | ListExpr -> [[Expr; ListExpr]; [Eps]]
      | Expr -> [[DEF; BlocFct; Valeur]; [DEF; ID; Valeur]; [Valeur]]
      | Valeur -> [[NB]; [LPAR; OP; Valeur; ResteVal]; [ID]; [BlocFct]]
      | ResteVal -> [[Valeur]; [Valeur; ResteVal]]
      | BlocFct -> [[LPAR; ID; Arg; RPAR]]
      | Arg -> [[ID; Arg]; [Eps]]
      | x -> [[x]]
;;

let PremG = [|
      [EOF; LPAR; NB; ID];
      [Eps; LPAR; NB; ID];
      [LPAR; NB; ID];
      [NB; LPAR; ID];
      [NB; LPAR; ID];
      [LPAR];
      [ID; Eps];
      [NB];
      [ID];
      [OP];
      [DEF];
      [LPAR];
      [RPAR];
      [EOF];
      [Eps]
   |]
;;
let SuivG = [|
