/* Parser */

/* Tokens */
%token LAM PI
%token <string> VAR
%token ARROW BOOL INT STAR
%token COMMA COLON
%token LPAREN RPAREN DOT
%token TURNSTILE
%token EOF

%start <bool option> check_derivation
%type  <Type.term> term
%type  <Type.t> t
%type  <(Type.term * Type.t) list> pseudo
%%

check_derivation:
  | EOF                                      { None }
  | pseudo TURNSTILE type_assign EOF
    { let (term, t) = $3 in
      Some (Assignment.check_derivation { pseudo=$1; term; t }) } ;

type_assign:
  | term COLON t                         { ($1, $3) } ;

t:
  | LPAREN t RPAREN                            { $2 }
  | t ARROW t                       { TFun ($1, $3) }
  | PI VAR COLON t DOT t         { TPi ($2, $4, $6) }
  | BOOL                                    { TBool }
  | INT                                      { TInt }
  | STAR                                    { TStar }
  | VAR                                   { TVar $1 } ;

term:
  | LPAREN term RPAREN                         { $2 }
  | term term                        { App ($1, $2) }
  | LAM VAR COLON t DOT term     { Lam ($2, $4, $6) }
  | VAR                                    { Var $1 } ;

pseudo:
  tas = separated_list(COMMA, type_assign)    { tas } ;