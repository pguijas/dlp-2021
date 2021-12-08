%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token BOOL
%token NAT
%token TPAIR
%token STRING

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token ARROW
%token UP
%token LBRACKET
%token COMMA
%token RBRACKET
%token RCORCHETE
%token LCORCHETE
%token QUOTE
%token EOF

%token <int> INTV
%token <string> STRINGV
%token <string> STRINGT

%start s
%type <Lambda.command> s
%type <Lambda.term> term

%%

s :
    term EOF
        { Eval $1 }
    | STRINGV EQ term
        { Bind ($1, $3) }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2,$4,$6)), $8) }

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }
  | atomicTerm DOT INTV
      { TmProj ($1, $3)}
  | atomicTerm UP atomicTerm
      { TmConcat ($1, $3)}

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | LBRACKET appTerm COMMA appTerm RBRACKET
      { TmPair ($2, $4) }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRINGT
    { TmString $1 }
  | LCORCHETE list { $2 }

list:
    RCORCHETE { TmEmptyList }
  | appTerm RCORCHETE { TmList($1,TmEmptyList) }
  | appTerm COMMA list { TmList($1,$3) }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | atomicTy TPAIR atomicTy
      { TyPair ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }