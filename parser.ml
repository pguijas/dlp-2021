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
\003\000\003\000\003\000\003\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\006\000\006\000\006\000\004\000\
\004\000\004\000\004\000\007\000\007\000\007\000\007\000\007\000\
\008\000\008\000\008\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\002\000\003\000\003\000\003\000\004\000\
\006\000\005\000\005\000\005\000\003\000\005\000\001\000\001\000\
\001\000\001\000\001\000\002\000\001\000\004\000\005\000\001\000\
\003\000\003\000\002\000\003\000\001\000\001\000\001\000\002\000\
\001\000\004\000\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\023\000\024\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\026\000\000\000\027\000\044\000\000\000\000\000\
\000\000\000\000\025\000\000\000\009\000\010\000\011\000\000\000\
\000\000\000\000\029\000\000\000\000\000\028\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\012\000\000\000\000\000\
\000\000\000\000\000\000\000\000\021\000\000\000\000\000\037\000\
\038\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\013\000\014\000\015\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\041\000\000\000\040\000\
\016\000\000\000\035\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\030\000\022\000\036\000\
\000\000\034\000\033\000\000\000\018\000\019\000\020\000\005\000\
\004\000\006\000\000\000\000\000\031\000\000\000\017\000\000\000\
\000\000\042\000\007\000\043\000"

let yydgoto = "\002\000\
\022\000\023\000\024\000\061\000\025\000\038\000\062\000\080\000"

let yysindex = "\015\000\
\090\255\000\000\221\254\000\000\000\000\128\255\214\255\214\255\
\214\255\223\254\242\254\128\255\034\255\252\254\006\255\022\255\
\026\255\028\255\000\000\037\255\000\000\000\000\060\000\214\255\
\010\255\040\255\000\000\058\255\000\000\000\000\000\000\055\255\
\057\255\076\255\000\000\075\255\208\255\000\000\015\255\015\255\
\015\255\015\255\015\255\128\255\000\000\000\000\012\255\214\255\
\015\255\128\255\128\255\015\255\000\000\165\255\165\255\000\000\
\000\000\000\000\015\255\236\254\077\255\004\255\079\255\081\255\
\082\255\083\255\000\000\000\000\000\000\000\000\084\255\097\255\
\100\255\092\255\182\255\211\255\095\255\000\000\094\255\000\000\
\000\000\015\255\000\000\015\255\214\255\214\255\214\255\214\255\
\128\255\128\255\128\255\128\255\237\254\000\000\000\000\000\000\
\015\255\000\000\000\000\214\255\000\000\000\000\000\000\000\000\
\000\000\000\000\105\255\075\255\000\000\027\255\000\000\128\255\
\236\254\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\000\000\000\000\000\000\000\003\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\168\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\054\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\248\255\228\255\002\000\025\000\037\000\012\000"

let yytablesize = 316
let yytable = "\028\000\
\008\000\026\000\003\000\032\000\037\000\034\000\078\000\035\000\
\029\000\030\000\031\000\063\000\064\000\065\000\066\000\001\000\
\079\000\108\000\082\000\083\000\071\000\025\000\033\000\074\000\
\039\000\046\000\084\000\056\000\057\000\047\000\077\000\058\000\
\059\000\048\000\040\000\004\000\005\000\067\000\046\000\060\000\
\007\000\008\000\009\000\072\000\073\000\075\000\076\000\068\000\
\069\000\070\000\041\000\012\000\113\000\114\000\042\000\099\000\
\043\000\044\000\013\000\045\000\035\000\049\000\050\000\014\000\
\015\000\016\000\017\000\018\000\110\000\019\000\036\000\021\000\
\032\000\032\000\032\000\051\000\046\000\046\000\052\000\032\000\
\032\000\032\000\104\000\105\000\106\000\107\000\100\000\101\000\
\102\000\103\000\003\000\004\000\005\000\006\000\053\000\054\000\
\007\000\008\000\009\000\010\000\011\000\111\000\090\000\089\000\
\081\000\115\000\085\000\012\000\086\000\087\000\088\000\091\000\
\092\000\096\000\013\000\097\000\112\000\109\000\098\000\014\000\
\015\000\016\000\017\000\018\000\116\000\019\000\020\000\021\000\
\003\000\004\000\005\000\006\000\000\000\000\000\007\000\008\000\
\009\000\010\000\011\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\000\000\000\000\000\000\000\000\014\000\015\000\016\000\
\017\000\018\000\000\000\019\000\027\000\021\000\004\000\005\000\
\000\000\025\000\025\000\007\000\008\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\012\000\004\000\
\005\000\025\000\000\000\025\000\000\000\013\000\000\000\025\000\
\025\000\025\000\014\000\015\000\016\000\017\000\018\000\012\000\
\019\000\027\000\021\000\025\000\025\000\025\000\013\000\093\000\
\094\000\004\000\005\000\000\000\004\000\005\000\000\000\004\000\
\005\000\019\000\027\000\021\000\000\000\000\000\000\000\000\000\
\000\000\012\000\000\000\000\000\012\000\000\000\000\000\012\000\
\013\000\055\000\000\000\013\000\000\000\095\000\013\000\000\000\
\000\000\000\000\000\000\019\000\027\000\021\000\019\000\027\000\
\021\000\019\000\027\000\021\000\000\000\000\000\000\000\000\000\
\000\000\000\000\008\000\008\000\000\000\008\000\008\000\003\000\
\003\000\000\000\000\000\000\000\008\000\000\000\003\000\000\000\
\000\000\000\000\008\000\008\000\000\000\003\000\000\000\025\000\
\025\000\008\000\008\000\008\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\008\000\008\000\008\000\025\000\
\000\000\025\000\000\000\000\000\000\000\025\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\000\025\000\025\000"

let yycheck = "\006\000\
\000\000\037\001\000\000\037\001\013\000\012\000\027\001\027\001\
\007\000\008\000\009\000\040\000\041\000\042\000\043\000\001\000\
\037\001\037\001\015\001\016\001\049\000\000\000\037\001\052\000\
\029\001\024\000\023\001\013\001\014\001\020\001\059\000\017\001\
\018\001\024\001\029\001\002\001\003\001\044\000\037\000\025\001\
\007\001\008\001\009\001\050\000\051\000\054\000\055\000\036\001\
\037\001\048\000\029\001\018\001\026\001\027\001\029\001\084\000\
\029\001\021\001\025\001\000\000\027\001\022\001\005\001\030\001\
\031\001\032\001\033\001\034\001\097\000\036\001\037\001\038\001\
\019\001\020\001\021\001\021\001\075\000\076\000\022\001\026\001\
\027\001\028\001\089\000\090\000\091\000\092\000\085\000\086\000\
\087\000\088\000\001\001\002\001\003\001\004\001\019\001\021\001\
\007\001\008\001\009\001\010\001\011\001\100\000\006\001\020\001\
\028\001\112\000\028\001\018\001\028\001\028\001\028\001\012\001\
\021\001\019\001\025\001\022\001\012\001\093\000\082\000\030\001\
\031\001\032\001\033\001\034\001\113\000\036\001\037\001\038\001\
\001\001\002\001\003\001\004\001\255\255\255\255\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\255\255\255\255\255\255\
\255\255\018\001\255\255\255\255\255\255\255\255\255\255\255\255\
\025\001\255\255\255\255\255\255\255\255\030\001\031\001\032\001\
\033\001\034\001\255\255\036\001\037\001\038\001\002\001\003\001\
\255\255\002\001\003\001\007\001\008\001\009\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\018\001\002\001\
\003\001\018\001\255\255\020\001\255\255\025\001\255\255\024\001\
\025\001\026\001\030\001\031\001\032\001\033\001\034\001\018\001\
\036\001\037\001\038\001\036\001\037\001\038\001\025\001\026\001\
\027\001\002\001\003\001\255\255\002\001\003\001\255\255\002\001\
\003\001\036\001\037\001\038\001\255\255\255\255\255\255\255\255\
\255\255\018\001\255\255\255\255\018\001\255\255\255\255\018\001\
\025\001\026\001\255\255\025\001\255\255\027\001\025\001\255\255\
\255\255\255\255\255\255\036\001\037\001\038\001\036\001\037\001\
\038\001\036\001\037\001\038\001\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\005\001\006\001\005\001\
\006\001\255\255\255\255\255\255\012\001\255\255\012\001\255\255\
\255\255\255\255\018\001\019\001\255\255\019\001\255\255\002\001\
\003\001\025\001\026\001\027\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\036\001\037\001\038\001\018\001\
\255\255\020\001\255\255\255\255\255\255\024\001\025\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\036\001\037\001\038\001"

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
# 303 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 57 "parser.mly"
        ( Bind (_1, _3) )
# 311 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 61 "parser.mly"
      ( _1 )
# 318 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Lambda.term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 63 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 327 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 65 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 336 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 67 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 345 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 69 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2,_4,_6)), _8) )
# 355 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 73 "parser.mly"
      ( _1 )
# 362 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 75 "parser.mly"
      ( TmSucc _2 )
# 369 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 77 "parser.mly"
      ( TmPred _2 )
# 376 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 79 "parser.mly"
      ( TmIsZero _2 )
# 383 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 81 "parser.mly"
      ( TmApp (_1, _2) )
# 391 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 83 "parser.mly"
      ( TmProj (_1, (string_of_int _3)))
# 399 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
      ( TmProj (_1, _3))
# 407 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 87 "parser.mly"
      ( TmConcat (_1, _3))
# 415 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 89 "parser.mly"
      ( TmNil _3 )
# 422 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 91 "parser.mly"
      ( TmCons (_3, _5, _6) )
# 431 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 93 "parser.mly"
      ( TmIsNil (_3, _5))
# 439 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 95 "parser.mly"
      ( TmHead (_3, _5))
# 447 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 97 "parser.mly"
      ( TmTail (_3, _5))
# 455 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lambda.term) in
    Obj.repr(
# 101 "parser.mly"
      ( _2 )
# 462 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'appTerm) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    Obj.repr(
# 103 "parser.mly"
      ( TmPair (_2, _4) )
# 470 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
      ( TmTrue )
# 476 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser.mly"
      ( TmFalse )
# 482 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
      ( TmVar _1 )
# 489 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 111 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 499 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "parser.mly"
    ( TmString _1 )
# 506 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'recordTM) in
    Obj.repr(
# 118 "parser.mly"
    ( TmRecord _2 )
# 513 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
              ( [] )
# 519 "parser.ml"
               : 'recordTM))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    Obj.repr(
# 122 "parser.mly"
                                 ( [(_1,_3)] )
# 527 "parser.ml"
               : 'recordTM))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'appTerm) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'recordTM) in
    Obj.repr(
# 123 "parser.mly"
                                       ( ((_1,_3)::(_5)) )
# 536 "parser.ml"
               : 'recordTM))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 127 "parser.mly"
      ( _1 )
# 543 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 129 "parser.mly"
      ( TyArr (_1, _3) )
# 551 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 131 "parser.mly"
      ( TyPair (_1, _3) )
# 559 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTy) in
    Obj.repr(
# 133 "parser.mly"
      ( TyList _1 )
# 566 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 137 "parser.mly"
      ( _2 )
# 573 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "parser.mly"
      ( TyBool )
# 579 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
      ( TyNat )
# 585 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
      ( TyString )
# 591 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'recordTY) in
    Obj.repr(
# 145 "parser.mly"
      ( TyRecord _2 )
# 598 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 148 "parser.mly"
              ( [] )
# 604 "parser.ml"
               : 'recordTY))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 149 "parser.mly"
                               ( [(_1,_3)] )
# 612 "parser.ml"
               : 'recordTY))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'recordTY) in
    Obj.repr(
# 150 "parser.mly"
                                     ( ((_1,_3)::(_5)) )
# 621 "parser.ml"
               : 'recordTY))
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
