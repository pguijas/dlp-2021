type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | LET
  | LETREC
  | IN
  | BOOL
  | NAT
  | TPAIR
  | LPAREN
  | RPAREN
  | DOT
  | EQ
  | COLON
  | ARROW
  | LBRACKET
  | COMMA
  | RBRACKET
  | EOF
  | INTV of (int)
  | STRINGV of (string)

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 35 "parser.ml"
let yytransl_const = [|
  257 (* LAMBDA *);
  258 (* TRUE *);
  259 (* FALSE *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* SUCC *);
  264 (* PRED *);
  265 (* ISZERO *);
  266 (* LET *);
  267 (* LETREC *);
  268 (* IN *);
  269 (* BOOL *);
  270 (* NAT *);
  271 (* TPAIR *);
  272 (* LPAREN *);
  273 (* RPAREN *);
  274 (* DOT *);
  275 (* EQ *);
  276 (* COLON *);
  277 (* ARROW *);
  278 (* LBRACKET *);
  279 (* COMMA *);
  280 (* RBRACKET *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  281 (* INTV *);
  282 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\003\000\003\000\003\000\005\000\005\000\005\000\
\005\000\005\000\005\000\004\000\004\000\004\000\006\000\006\000\
\006\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\006\000\006\000\006\000\008\000\003\000\
\001\000\002\000\002\000\002\000\002\000\003\000\005\000\001\000\
\001\000\001\000\001\000\001\000\003\000\003\000\003\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\016\000\017\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\000\000\026\000\
\000\000\000\000\009\000\000\000\018\000\000\000\010\000\011\000\
\012\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\013\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\008\000\024\000\025\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\015\000\023\000\000\000\022\000\021\000\000\000\000\000\
\000\000\000\000\000\000"

let yydgoto = "\002\000\
\016\000\017\000\018\000\045\000\019\000\046\000"

let yysindex = "\006\000\
\031\255\000\000\246\254\000\000\000\000\060\255\102\255\102\255\
\102\255\248\254\000\255\060\255\071\255\000\000\039\255\000\000\
\004\000\102\255\000\000\035\255\000\000\003\255\000\000\000\000\
\000\000\053\255\046\255\027\255\086\255\060\255\050\255\000\000\
\000\000\001\255\060\255\060\255\001\255\000\000\071\255\059\255\
\000\000\000\000\000\000\001\255\063\255\022\255\007\255\036\255\
\064\255\097\255\067\255\060\255\001\255\001\255\060\255\060\255\
\060\255\000\000\000\000\059\255\000\000\000\000\059\255\059\255\
\047\255\060\255\059\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\094\000\
\000\000\000\000\000\000\000\000\000\000\073\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\000\000\000\000\023\000\027\000\
\000\000\000\000\036\000"

let yygindex = "\000\000\
\000\000\250\255\245\255\224\255\002\000\042\000"

let yytablesize = 309
let yytable = "\022\000\
\018\000\029\000\003\000\032\000\049\000\028\000\001\000\035\000\
\023\000\024\000\025\000\051\000\055\000\042\000\043\000\020\000\
\044\000\026\000\005\000\033\000\031\000\062\000\004\000\040\000\
\031\000\027\000\006\000\050\000\047\000\048\000\033\000\003\000\
\004\000\005\000\006\000\007\000\053\000\007\000\008\000\009\000\
\010\000\011\000\054\000\038\000\031\000\060\000\012\000\056\000\
\063\000\064\000\065\000\033\000\013\000\031\000\034\000\014\000\
\015\000\030\000\066\000\067\000\003\000\004\000\005\000\006\000\
\031\000\037\000\007\000\008\000\009\000\010\000\011\000\036\000\
\004\000\005\000\041\000\012\000\031\000\007\000\008\000\009\000\
\052\000\013\000\057\000\059\000\014\000\021\000\012\000\004\000\
\005\000\020\000\020\000\020\000\013\000\002\000\061\000\014\000\
\021\000\000\000\004\000\005\000\000\000\012\000\000\000\004\000\
\005\000\000\000\000\000\013\000\039\000\000\000\014\000\021\000\
\012\000\000\000\000\000\000\000\000\000\012\000\013\000\000\000\
\058\000\014\000\021\000\013\000\000\000\000\000\014\000\021\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\018\000\018\000\000\000\000\000\000\000\003\000\
\003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
\018\000\000\000\018\000\003\000\003\000\031\000\018\000\005\000\
\005\000\018\000\018\000\004\000\004\000\000\000\005\000\006\000\
\006\000\000\000\004\000\005\000\000\000\000\000\006\000\004\000\
\007\000\007\000\000\000\006\000\000\000\000\000\000\000\007\000\
\000\000\000\000\000\000\000\000\007\000"

let yycheck = "\006\000\
\000\000\013\000\000\000\000\000\037\000\012\000\001\000\005\001\
\007\000\008\000\009\000\044\000\006\001\013\001\014\001\026\001\
\016\001\026\001\000\000\018\000\018\001\054\000\000\000\030\000\
\018\001\026\001\000\000\039\000\035\000\036\000\029\000\001\001\
\002\001\003\001\004\001\000\000\015\001\007\001\008\001\009\001\
\010\001\011\001\021\001\017\001\018\001\052\000\016\001\012\001\
\055\000\056\000\057\000\050\000\022\001\018\001\020\001\025\001\
\026\001\019\001\012\001\066\000\001\001\002\001\003\001\004\001\
\018\001\020\001\007\001\008\001\009\001\010\001\011\001\019\001\
\002\001\003\001\025\001\016\001\018\001\007\001\008\001\009\001\
\018\001\022\001\019\001\017\001\025\001\026\001\016\001\002\001\
\003\001\017\001\018\001\019\001\022\001\000\000\053\000\025\001\
\026\001\255\255\002\001\003\001\255\255\016\001\255\255\002\001\
\003\001\255\255\255\255\022\001\023\001\255\255\025\001\026\001\
\016\001\255\255\255\255\255\255\255\255\016\001\022\001\255\255\
\024\001\025\001\026\001\022\001\255\255\255\255\025\001\026\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\255\255\255\255\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\012\001\255\255\
\016\001\255\255\018\001\017\001\018\001\018\001\022\001\005\001\
\006\001\025\001\026\001\005\001\006\001\255\255\012\001\005\001\
\006\001\255\255\012\001\017\001\255\255\255\255\012\001\017\001\
\005\001\006\001\255\255\017\001\255\255\255\255\255\255\012\001\
\255\255\255\255\255\255\255\255\017\001"

let yynames_const = "\
  LAMBDA\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  LET\000\
  LETREC\000\
  IN\000\
  BOOL\000\
  NAT\000\
  TPAIR\000\
  LPAREN\000\
  RPAREN\000\
  DOT\000\
  EQ\000\
  COLON\000\
  ARROW\000\
  LBRACKET\000\
  COMMA\000\
  RBRACKET\000\
  EOF\000\
  "

let yynames_block = "\
  INTV\000\
  STRINGV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lambda.term) in
    Obj.repr(
# 44 "parser.mly"
        ( Eval _1 )
# 243 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 46 "parser.mly"
        ( Bind (_1, _3) )
# 251 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 51 "parser.mly"
      ( _1 )
# 258 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Lambda.term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 53 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 267 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 55 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 276 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 57 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 285 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 59 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2,_4,_6)), _8) )
# 295 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 61 "parser.mly"
      ( TmProj (_1, _3) )
# 303 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 66 "parser.mly"
      ( _1 )
# 310 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 68 "parser.mly"
      ( TmSucc _2 )
# 317 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 70 "parser.mly"
      ( TmPred _2 )
# 324 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 72 "parser.mly"
      ( TmIsZero _2 )
# 331 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 74 "parser.mly"
      ( TmApp (_1, _2) )
# 339 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lambda.term) in
    Obj.repr(
# 78 "parser.mly"
      ( _2 )
# 346 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'appTerm) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    Obj.repr(
# 80 "parser.mly"
      ( TmPair (_2, _4) )
# 354 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
      ( TmTrue )
# 360 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
      ( TmFalse )
# 366 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
      ( TmVar _1 )
# 373 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 88 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 383 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 95 "parser.mly"
      ( _1 )
# 390 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 97 "parser.mly"
      ( TyArr (_1, _3) )
# 398 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 99 "parser.mly"
      ( TyPair (_1, _3) )
# 406 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 103 "parser.mly"
      ( _2 )
# 413 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
      ( TyBool )
# 419 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser.mly"
      ( TyNat )
# 425 "parser.ml"
               : 'atomicTy))
(* Entry s *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.command)
