/* Analyse Syntaxique */

%token <string> NUM STR
%token PLUS MOINS FOIS DIV
%token LPAR RPAR
%start Main
%type <string> Main
%%

Main :
	LPAR Expr RPAR  { $2 }
;

Expr :
        NUM				{ $1 ^ " " }
  | LPAR Expr RPAR		{ $2 }
  | PLUS Expr Expr		{ $2 ^ $3 ^ "+ " }
  | MOINS Expr Expr		{ $2 ^ $3 ^ "- " }
  | FOIS Expr Expr		{ $2 ^ $3 ^ "* " }
  | DIV Expr Expr		{ $2 ^ $3 ^ "/ " }
;;
  
