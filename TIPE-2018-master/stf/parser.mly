/***********************************************************************************/
/*                              TIPE 2018 - Compiling                              */
/*           Nestor Laborier   Kineider Guillaume   Bourret-Mathieu Blaise         */
/*                                                                                 */
/*                    SYNTAXICAL ANALYSIS WITH THE CAMLYACC TOOL                   */
/*                                                                                 */
/*                                                                                 */
/*                    This file is published under WTFPL License                   */
/***********************************************************************************/

   %token <string> NUM
   %token PLUS MOINS FOIS DIV
   %token LPAR RPAR
   %token EOL
   %nonassoc UMINUS
   %start Main
   %type <string> Main
   %%

   Main :
    LPAR Expr RPAR EOL               { $2 }
   ;

   Expr :
         NUM                        { $1 ^ " " }
       | LPAR Expr RPAR             { $2 }
       | PLUS Expr Expr             { $2 ^ $3 ^ "+ " }
       | MOINS Expr Expr            { $2 ^ $3 ^ "- " }
       | FOIS Expr Expr             { $2 ^ $3 ^ "* " }
       | DIV Expr Expr              { $2 ^ $3 ^ "/ " }
;;
  
