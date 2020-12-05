/* Parser */

/* Tokens */
%token LAM PI
%token <string> VAR
%token <int> DIGITS
%token LPAREN RPAREN
%token ARROW BOOL INT STAR
%token FALSE TRUE
%token COMMA COLON
%token TURNSTILE NTURNSTILE DOT
%token EOF

%start <bool option> check_derivation
%type  <Type.term> term
%type  <Type.t> t
%type  <(Type.term * Type.t) list> pseudo
%%

check_derivation:
  | EOF                                      { None }
  | pseudo TURNSTILE type_assign EOF
    { let term, t = $3 in
      let open Assignment.Derive in
      Some (check_derivation { pseudo=$1; term; t }) }
  | pseudo NTURNSTILE type_assign EOF
    { let term, t = $3 in
      let open Assignment.Derive in
      Some (not (check_derivation { pseudo=$1; term; t })) } ;

type_assign:
  | term COLON t
    { let open Vars in
      let x, t = $1, $3 in
      if t = TStar then
        (begin match x with
        | Var v -> decl_type_vars @= v
        | _ -> () (* TODO *)
        end ;
        (x, t))
      else
        let tvars' = Type.tvars t in
        try
          if Vars.check_tvars tvars' then
          begin match x with
          | Var v -> decl_type_vars @= v
          | _ -> () (* TODO *)
          end ;
          (x, t)
        with
        | Failure f ->
            (print_endline f ; (x, TBad x)) } ;

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
  | TRUE                                { BLit true }
  | FALSE                              { BLit false }
  | DIGITS                                { ILit $1 }
  | VAR                                    { Var $1 } ;

pseudo:
  tas = separated_list(COMMA, type_assign)    { tas } ;
