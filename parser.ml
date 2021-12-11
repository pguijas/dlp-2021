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
  | LIST
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
  | NIL
  | CONS
  | ISNIL
  | HEAD
  | TAIL
  | QUOTE
  | EOF
  | INTV of (int)
  | STRINGV of (string)
  | STRINGT of (string)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Lambda;;
# 47 "parser.ml"
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
  272 (* LIST *);
  273 (* STRING *);
  274 (* LPAREN *);
  275 (* RPAREN *);
  276 (* DOT *);
  277 (* EQ *);
  278 (* COLON *);
  279 (* ARROW *);
  280 (* UP *);
  281 (* LBRACKET *);
  282 (* COMMA *);
  283 (* RBRACKET *);
  284 (* RCORCHETE *);
  285 (* LCORCHETE *);
  286 (* NIL *);
  287 (* CONS *);
  288 (* ISNIL *);
  289 (* HEAD *);
  290 (* TAIL *);
  291 (* QUOTE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  292 (* INTV *);
  293 (* STRINGV *);
  294 (* STRINGT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\004\000\004\000\004\000\004\000\006\000\006\000\
\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\002\000\003\000\003\000\004\000\006\000\
\005\000\005\000\005\000\003\000\005\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\002\000\003\000\001\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\022\000\023\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\025\000\000\000\026\000\035\000\000\000\000\000\
\000\000\000\000\024\000\000\000\009\000\010\000\011\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\012\000\000\000\000\000\000\000\000\000\
\000\000\020\000\000\000\032\000\033\000\034\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\014\000\013\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\000\000\
\030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\000\031\000\029\000\028\000\000\000\017\000\
\018\000\019\000\005\000\004\000\006\000\000\000\016\000\000\000\
\007\000"

let yydgoto = "\002\000\
\022\000\023\000\024\000\056\000\025\000\057\000"

let yysindex = "\004\000\
\021\255\000\000\223\254\000\000\000\000\084\255\176\255\176\255\
\176\255\235\254\236\254\084\255\131\255\245\254\246\254\247\254\
\004\255\005\255\000\000\015\255\000\000\000\000\038\000\152\255\
\023\255\022\255\000\000\044\255\000\000\000\000\000\000\029\255\
\034\255\043\255\148\255\047\255\047\255\047\255\047\255\047\255\
\084\255\000\000\176\255\000\000\030\255\047\255\084\255\084\255\
\047\255\000\000\131\255\000\000\000\000\000\000\047\255\039\255\
\248\254\040\255\056\255\068\255\069\255\000\000\000\000\000\000\
\078\255\093\255\088\255\080\255\045\255\085\255\000\000\047\255\
\000\000\047\255\176\255\176\255\176\255\176\255\084\255\084\255\
\084\255\084\255\000\000\000\000\000\000\000\000\176\255\000\000\
\000\000\000\000\000\000\000\000\000\000\091\255\000\000\084\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\027\000\000\000\000\000\000\000\003\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\249\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\250\255\245\255\086\000\002\000\033\000"

let yytablesize = 321
let yytable = "\028\000\
\008\000\035\000\003\000\026\000\001\000\034\000\072\000\073\000\
\029\000\030\000\031\000\027\000\027\000\027\000\074\000\032\000\
\033\000\036\000\037\000\038\000\027\000\003\000\004\000\005\000\
\006\000\044\000\024\000\007\000\008\000\009\000\010\000\011\000\
\039\000\040\000\062\000\041\000\044\000\042\000\012\000\069\000\
\066\000\067\000\045\000\046\000\063\000\013\000\004\000\005\000\
\047\000\048\000\014\000\015\000\016\000\017\000\018\000\049\000\
\019\000\020\000\021\000\052\000\053\000\050\000\012\000\054\000\
\055\000\064\000\071\000\075\000\043\000\013\000\044\000\083\000\
\091\000\092\000\093\000\094\000\087\000\088\000\089\000\090\000\
\019\000\027\000\021\000\076\000\003\000\004\000\005\000\006\000\
\095\000\097\000\007\000\008\000\009\000\010\000\011\000\077\000\
\078\000\079\000\080\000\081\000\082\000\012\000\096\000\084\000\
\085\000\000\000\000\000\000\000\013\000\000\000\000\000\000\000\
\000\000\014\000\015\000\016\000\017\000\018\000\000\000\019\000\
\027\000\021\000\058\000\059\000\060\000\061\000\000\000\000\000\
\000\000\000\000\000\000\065\000\004\000\005\000\068\000\000\000\
\000\000\007\000\008\000\009\000\070\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000\004\000\005\000\000\000\
\000\000\004\000\005\000\013\000\000\000\000\000\000\000\086\000\
\014\000\015\000\016\000\017\000\018\000\012\000\019\000\027\000\
\021\000\012\000\000\000\043\000\013\000\051\000\000\000\043\000\
\013\000\004\000\005\000\000\000\000\000\000\000\000\000\019\000\
\027\000\021\000\000\000\019\000\027\000\021\000\000\000\000\000\
\000\000\012\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\027\000\021\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\008\000\008\000\000\000\008\000\008\000\003\000\
\003\000\000\000\000\000\000\000\008\000\000\000\003\000\000\000\
\000\000\000\000\008\000\008\000\000\000\003\000\000\000\000\000\
\008\000\008\000\008\000\008\000\024\000\024\000\000\000\000\000\
\000\000\000\000\000\000\000\000\008\000\008\000\008\000\000\000\
\000\000\000\000\000\000\000\000\024\000\000\000\024\000\000\000\
\000\000\000\000\024\000\024\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\024\000\024\000\
\024\000"

let yycheck = "\006\000\
\000\000\013\000\000\000\037\001\001\000\012\000\015\001\016\001\
\007\000\008\000\009\000\019\001\020\001\021\001\023\001\037\001\
\037\001\029\001\029\001\029\001\028\001\001\001\002\001\003\001\
\004\001\024\000\000\000\007\001\008\001\009\001\010\001\011\001\
\029\001\029\001\041\000\021\001\035\000\000\000\018\001\051\000\
\047\000\048\000\020\001\022\001\043\000\025\001\002\001\003\001\
\005\001\021\001\030\001\031\001\032\001\033\001\034\001\022\001\
\036\001\037\001\038\001\013\001\014\001\019\001\018\001\017\001\
\018\001\036\001\028\001\028\001\024\001\025\001\069\000\027\001\
\079\000\080\000\081\000\082\000\075\000\076\000\077\000\078\000\
\036\001\037\001\038\001\028\001\001\001\002\001\003\001\004\001\
\087\000\096\000\007\001\008\001\009\001\010\001\011\001\028\001\
\028\001\020\001\006\001\012\001\021\001\018\001\012\001\019\001\
\072\000\255\255\255\255\255\255\025\001\255\255\255\255\255\255\
\255\255\030\001\031\001\032\001\033\001\034\001\255\255\036\001\
\037\001\038\001\037\000\038\000\039\000\040\000\255\255\255\255\
\255\255\255\255\255\255\046\000\002\001\003\001\049\000\255\255\
\255\255\007\001\008\001\009\001\055\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\018\001\002\001\003\001\255\255\
\255\255\002\001\003\001\025\001\255\255\255\255\255\255\074\000\
\030\001\031\001\032\001\033\001\034\001\018\001\036\001\037\001\
\038\001\018\001\255\255\024\001\025\001\026\001\255\255\024\001\
\025\001\002\001\003\001\255\255\255\255\255\255\255\255\036\001\
\037\001\038\001\255\255\036\001\037\001\038\001\255\255\255\255\
\255\255\018\001\255\255\255\255\255\255\255\255\255\255\255\255\
\025\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\036\001\037\001\038\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\005\001\006\001\005\001\
\006\001\255\255\255\255\255\255\012\001\255\255\012\001\255\255\
\255\255\255\255\018\001\019\001\255\255\019\001\255\255\255\255\
\024\001\025\001\026\001\027\001\002\001\003\001\255\255\255\255\
\255\255\255\255\255\255\255\255\036\001\037\001\038\001\255\255\
\255\255\255\255\255\255\255\255\018\001\255\255\020\001\255\255\
\255\255\255\255\024\001\025\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\036\001\037\001\
\038\001"

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
  LIST\000\
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
  NIL\000\
  CONS\000\
  ISNIL\000\
  HEAD\000\
  TAIL\000\
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
# 55 "parser.mly"
        ( Eval _1 )
# 297 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 57 "parser.mly"
        ( Bind (_1, _3) )
# 305 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 61 "parser.mly"
      ( _1 )
# 312 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Lambda.term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 63 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 321 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 65 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 330 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 67 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 339 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 69 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2,_4,_6)), _8) )
# 349 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 73 "parser.mly"
      ( _1 )
# 356 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 75 "parser.mly"
      ( TmSucc _2 )
# 363 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 77 "parser.mly"
      ( TmPred _2 )
# 370 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 79 "parser.mly"
      ( TmIsZero _2 )
# 377 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 81 "parser.mly"
      ( TmApp (_1, _2) )
# 385 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 83 "parser.mly"
      ( TmProj (_1, _3))
# 393 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'appTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 85 "parser.mly"
      ( TmConcat (_1, _3))
# 401 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 87 "parser.mly"
      ( TmNil _3 )
# 408 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 89 "parser.mly"
      ( TmCons (_3, _5, _6) )
# 417 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 91 "parser.mly"
      ( TmIsNil (_3, _5))
# 425 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 93 "parser.mly"
      ( TmHead (_3, _5))
# 433 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 95 "parser.mly"
      ( TmTail (_3, _5))
# 441 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lambda.term) in
    Obj.repr(
# 99 "parser.mly"
      ( _2 )
# 448 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'appTerm) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    Obj.repr(
# 101 "parser.mly"
      ( TmPair (_2, _4) )
# 456 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
      ( TmTrue )
# 462 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
      ( TmFalse )
# 468 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 107 "parser.mly"
      ( TmVar _1 )
# 475 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 109 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 485 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 114 "parser.mly"
    ( TmString _1 )
# 492 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 118 "parser.mly"
      ( _1 )
# 499 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 120 "parser.mly"
      ( TyArr (_1, _3) )
# 507 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 122 "parser.mly"
      ( TyPair (_1, _3) )
# 515 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTy) in
    Obj.repr(
# 124 "parser.mly"
      ( TyList _1 )
# 522 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 128 "parser.mly"
      ( _2 )
# 529 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "parser.mly"
      ( TyBool )
# 535 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
      ( TyNat )
# 541 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "parser.mly"
      ( TyString )
# 547 "parser.ml"
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
