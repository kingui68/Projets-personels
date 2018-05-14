/* Analyse Syntaxique */

   %{
  #open "make_forth";;
  %}
     
%token <string> NUM
%token <string list> DEF
%token <string> WRD
%token PLUS MOINS FOIS DIV APP
%token LPAR RPAR
%start Main
%type <string> Main
%%

Main :
	LPAR Expr RPAR  { $2 }
;

Expr :
    NUM { $1 } 
  | WRD { $1 }
  | LPAR WRD Expr RPAR { app $2 $3 }
  | LPAR Expr RPAR { $2 }
  | Expr Expr {" "^$2^" "^$1 }
  | DEF Expr { make_forth $1 $2 }
  | WRD Expr { app $1 $2 }
;;
  
