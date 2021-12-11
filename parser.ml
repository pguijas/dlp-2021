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
  | STRING
  | LPAREN
  | RPAREN
  | DOT
  | EQ
  | COLON
  | ARROW
  | UP
  | LBRACKET
  | COMMA
  | RBRACKET
  | RCORCHETE
  | LCORCHETE
  | QUOTE
  | EOF
  | INTV of (int)
  | STRINGV of (string)
  | STRINGT of (string)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Lambda;;
# 41 "parser.ml"
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
  272 (* STRING *);
  273 (* LPAREN *);
  274 (* RPAREN *);
  275 (* DOT *);
  276 (* EQ *);
  277 (* COLON *);
  278 (* ARROW *);
  279 (* UP *);
  280 (* LBRACKET *);
  281 (* COMMA *);
  282 (* RBRACKET *);
  283 (* RCORCHETE *);
  284 (* LCORCHETE *);
  285 (* QUOTE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  286 (* INTV *);
  287 (* STRINGV *);
  288 (* STRINGT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\006\000\006\000\
\006\000\004\000\004\000\004\000\007\000\007\000\007\000\007\000\
\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\002\000\003\000\003\000\003\000\005\000\
\001\000\001\000\001\000\001\000\001\000\002\000\001\000\002\000\
\003\000\001\000\003\000\003\000\003\000\001\000\001\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\017\000\018\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000\000\000\
\021\000\033\000\000\000\000\000\000\000\000\000\019\000\000\000\
\009\000\010\000\011\000\000\000\000\000\000\000\000\000\023\000\
\000\000\022\000\000\000\001\000\000\000\012\000\000\000\000\000\
\000\000\000\000\000\000\015\000\000\000\000\000\024\000\002\000\
\014\000\013\000\030\000\031\000\032\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\025\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\029\000\005\000\028\000\
\027\000\004\000\006\000\000\000\000\000\007\000"

let yydgoto = "\002\000\
\018\000\019\000\020\000\055\000\021\000\034\000\056\000"

let yysindex = "\010\000\
\066\255\000\000\227\254\000\000\000\000\078\255\171\255\171\255\
\171\255\230\254\232\254\078\255\014\255\109\255\000\000\248\254\
\000\000\000\000\019\000\152\255\001\255\009\255\000\000\027\255\
\000\000\000\000\000\000\013\255\030\255\016\255\128\255\000\000\
\097\255\000\000\078\255\000\000\171\255\000\000\020\255\011\255\
\078\255\078\255\011\255\000\000\014\255\109\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\255\033\255\244\254\
\047\255\042\255\035\255\140\255\000\000\038\255\078\255\011\255\
\011\255\078\255\078\255\078\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\046\255\078\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\000\000\000\000\000\000\004\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\251\255\050\000\006\000\013\000\255\255"

let yytablesize = 306
let yytable = "\024\000\
\008\000\022\000\064\000\003\000\028\000\030\000\029\000\031\000\
\033\000\065\000\001\000\035\000\025\000\026\000\027\000\004\000\
\005\000\019\000\036\000\039\000\007\000\008\000\009\000\051\000\
\052\000\038\000\053\000\054\000\048\000\040\000\012\000\041\000\
\042\000\044\000\057\000\058\000\038\000\013\000\038\000\060\000\
\033\000\014\000\049\000\015\000\023\000\017\000\026\000\026\000\
\026\000\050\000\043\000\063\000\066\000\067\000\068\000\070\000\
\071\000\077\000\061\000\074\000\075\000\076\000\072\000\000\000\
\000\000\038\000\003\000\004\000\005\000\006\000\078\000\000\000\
\007\000\008\000\009\000\010\000\011\000\000\000\003\000\004\000\
\005\000\006\000\012\000\000\000\007\000\008\000\009\000\010\000\
\011\000\013\000\000\000\000\000\059\000\014\000\012\000\015\000\
\016\000\017\000\004\000\005\000\000\000\013\000\000\000\062\000\
\000\000\014\000\000\000\015\000\023\000\017\000\004\000\005\000\
\000\000\012\000\073\000\007\000\008\000\009\000\000\000\037\000\
\013\000\046\000\000\000\047\000\014\000\012\000\015\000\023\000\
\017\000\004\000\005\000\000\000\013\000\000\000\000\000\032\000\
\014\000\000\000\015\000\023\000\017\000\004\000\005\000\000\000\
\012\000\000\000\000\000\000\000\000\000\000\000\037\000\013\000\
\045\000\004\000\005\000\014\000\012\000\015\000\023\000\017\000\
\000\000\000\000\037\000\013\000\000\000\069\000\000\000\014\000\
\012\000\015\000\023\000\017\000\004\000\005\000\037\000\013\000\
\000\000\000\000\000\000\014\000\000\000\015\000\023\000\017\000\
\000\000\000\000\000\000\012\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\000\000\000\000\014\000\000\000\
\015\000\023\000\017\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\008\000\008\000\000\000\008\000\008\000\000\000\
\003\000\003\000\000\000\000\000\008\000\000\000\000\000\003\000\
\000\000\008\000\008\000\019\000\019\000\003\000\000\000\008\000\
\008\000\008\000\008\000\008\000\008\000\000\000\008\000\008\000\
\008\000\000\000\019\000\000\000\019\000\000\000\000\000\000\000\
\019\000\019\000\000\000\000\000\000\000\019\000\000\000\019\000\
\019\000\019\000"

let yycheck = "\006\000\
\000\000\031\001\015\001\000\000\031\001\012\000\031\001\013\000\
\014\000\022\001\001\000\020\001\007\000\008\000\009\000\002\001\
\003\001\000\000\000\000\019\001\007\001\008\001\009\001\013\001\
\014\001\020\000\016\001\017\001\035\000\021\001\017\001\005\001\
\020\001\018\001\041\000\042\000\031\000\024\001\033\000\045\000\
\046\000\028\001\037\000\030\001\031\001\032\001\018\001\019\001\
\020\001\030\001\021\001\019\001\006\001\012\001\020\001\018\001\
\063\000\012\001\046\000\066\000\067\000\068\000\064\000\255\255\
\255\255\060\000\001\001\002\001\003\001\004\001\077\000\255\255\
\007\001\008\001\009\001\010\001\011\001\255\255\001\001\002\001\
\003\001\004\001\017\001\255\255\007\001\008\001\009\001\010\001\
\011\001\024\001\255\255\255\255\043\000\028\001\017\001\030\001\
\031\001\032\001\002\001\003\001\255\255\024\001\255\255\054\000\
\255\255\028\001\255\255\030\001\031\001\032\001\002\001\003\001\
\255\255\017\001\065\000\007\001\008\001\009\001\255\255\023\001\
\024\001\025\001\255\255\027\001\028\001\017\001\030\001\031\001\
\032\001\002\001\003\001\255\255\024\001\255\255\255\255\027\001\
\028\001\255\255\030\001\031\001\032\001\002\001\003\001\255\255\
\017\001\255\255\255\255\255\255\255\255\255\255\023\001\024\001\
\025\001\002\001\003\001\028\001\017\001\030\001\031\001\032\001\
\255\255\255\255\023\001\024\001\255\255\026\001\255\255\028\001\
\017\001\030\001\031\001\032\001\002\001\003\001\023\001\024\001\
\255\255\255\255\255\255\028\001\255\255\030\001\031\001\032\001\
\255\255\255\255\255\255\017\001\255\255\255\255\255\255\255\255\
\255\255\255\255\024\001\255\255\255\255\255\255\028\001\255\255\
\030\001\031\001\032\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\005\001\006\001\255\255\
\005\001\006\001\255\255\255\255\012\001\255\255\255\255\012\001\
\255\255\017\001\018\001\002\001\003\001\018\001\255\255\023\001\
\024\001\025\001\026\001\027\001\028\001\255\255\030\001\031\001\
\032\001\255\255\017\001\255\255\019\001\255\255\255\255\255\255\
\023\001\024\001\255\255\255\255\255\255\028\001\255\255\030\001\
\031\001\032\001"

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
  STRING\000\
  LPAREN\000\
  RPAREN\000\
  DOT\000\
  EQ\000\
  COLON\000\
  ARROW\000\
  UP\000\
  LBRACKET\000\
  COMMA\000\
  RBRACKET\000\
  RCORCHETE\000\
  LCORCHETE\000\
  QUOTE\000\
  EOF\000\
  "

let yynames_block = "\
  INTV\000\
  STRINGV\000\
  STRINGT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lambda.term) in
    Obj.repr(
# 49 "parser.mly"
        ( Eval _1 )
# 266 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 51 "parser.mly"
        ( Bind (_1, _3) )
# 274 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 55 "parser.mly"
      ( _1 )
# 281 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Lambda.term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 57 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 290 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 59 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 299 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 61 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 308 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 63 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2,_4,_6)), _8) )
# 318 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 67 "parser.mly"
      ( _1 )
# 325 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 69 "parser.mly"
      ( TmSucc _2 )
# 332 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 71 "parser.mly"
      ( TmPred _2 )
# 339 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 73 "parser.mly"
      ( TmIsZero _2 )
# 346 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 75 "parser.mly"
      ( TmApp (_1, _2) )
# 354 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 77 "parser.mly"
      ( TmProj (_1, _3))
# 362 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'appTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 79 "parser.mly"
      ( TmConcat (_1, _3))
# 370 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lambda.term) in
    Obj.repr(
# 83 "parser.mly"
      ( _2 )
# 377 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'appTerm) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    Obj.repr(
# 85 "parser.mly"
      ( TmPair (_2, _4) )
# 385 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
      ( TmTrue )
# 391 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
      ( TmFalse )
# 397 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "parser.mly"
      ( TmVar _1 )
# 404 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 93 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 414 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "parser.mly"
    ( TmString _1 )
# 421 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 99 "parser.mly"
                   ( _2 )
# 428 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
              ( TmEmptyList )
# 434 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    Obj.repr(
# 103 "parser.mly"
                      ( TmList(_1,TmEmptyList) )
# 441 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'appTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 104 "parser.mly"
                       ( TmList(_1,_3) )
# 449 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 108 "parser.mly"
      ( _1 )
# 456 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 110 "parser.mly"
      ( TyArr (_1, _3) )
# 464 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 112 "parser.mly"
      ( TyPair (_1, _3) )
# 472 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 116 "parser.mly"
      ( _2 )
# 479 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
      ( TyBool )
# 485 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
      ( TyNat )
# 491 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
      ( TyString )
# 497 "parser.ml"
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
