%{
let make_fun vars expr =
  List.fold_right (fun v e -> Syntax.Fun (v, e)) vars expr
%}
/* token definition */
%token LPAREN RPAREN
%token <int> NUMBER
%token <string> VAR
%token PLUS MINUS TIMES DIVIDE
%token FUN ARROW
%token SHIFT CONTROL SHIFT0 CONTROL0 RESET
%token EOF
/* End of File */

/* terminal symbol */
%type <Syntax.e> expr

/* start symbol */
%start expr

%nonassoc ARROW
%left PLUS MINUS
%left TIMES DIVIDE

/* Don't omitte '%%' */
%%

/* Write grammar rules */  
  
simple_expr:
| NUMBER
{ Syntax.Num ($1) }
| VAR
{ Syntax.Var ($1) }
| LPAREN expr RPAREN
{ $2 }

expr:
| simple_expr
{ $1 }
| expr PLUS expr
        { Syntax.Op ($1, Syntax.Plus, $3) }
| expr MINUS expr
        { Syntax.Op ($1, Syntax.Minus, $3) }
| expr TIMES expr
        { Syntax.Op ($1, Syntax.Times, $3) }
| expr DIVIDE expr
        { Syntax.Op ($1, Syntax.Divide, $3) }
| FUN VAR vars ARROW expr
{ Syntax.Fun ($2, make_fun $3 $5) }
| app
{ $1 }
| SHIFT VAR ARROW expr
{ Syntax.Shift ($2, $4) }
| CONTROL VAR ARROW expr
{ Syntax.Control ($2, $4) }
| SHIFT0 VAR ARROW expr
{ Syntax.Shift0 ($2, $4) }
| CONTROL0 VAR ARROW expr
{ Syntax.Control0 ($2, $4) }
| RESET simple_expr
{ Syntax.Reset ($2) }

vars:
|
  { [] }
| VAR vars
{ $1 :: $2 }

app:
| simple_expr simple_expr
{ Syntax.App ($1, $2) }
| app simple_expr
{ Syntax.App ($1, $2) }
