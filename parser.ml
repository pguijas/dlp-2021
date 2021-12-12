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
\000\000\000\000\000\000\000\000\001\000\000\000\012\000\000\000\
\000\000\000\000\000\000\000\000\021\000\000\000\000\000\037\000\
\038\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\015\000\013\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\041\000\000\000\040\000\
\016\000\000\000\035\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\030\000\022\000\036\000\
\000\000\034\000\033\000\000\000\018\000\019\000\020\000\005\000\
\004\000\006\000\000\000\000\000\031\000\000\000\017\000\000\000\
\000\000\042\000\007\000\043\000"

let yydgoto = "\002\000\
\022\000\023\000\024\000\061\000\025\000\038\000\062\000\080\000"

let yysindex = "\007\000\
\090\255\000\000\221\254\000\000\000\000\128\255\236\255\236\255\
\236\255\223\254\235\254\128\255\034\255\247\254\022\255\024\255\
\025\255\026\255\000\000\013\255\000\000\000\000\057\000\056\000\
\038\255\040\255\000\000\055\255\000\000\000\000\000\000\042\255\
\057\255\076\255\000\000\075\255\211\255\000\000\005\255\005\255\
\005\255\005\255\005\255\128\255\000\000\236\255\000\000\248\254\
\005\255\128\255\128\255\005\255\000\000\165\255\165\255\000\000\
\000\000\000\000\005\255\236\254\048\255\017\255\077\255\079\255\
\081\255\082\255\000\000\000\000\000\000\000\000\083\255\098\255\
\099\255\091\255\182\255\030\000\094\255\000\000\092\255\000\000\
\000\000\005\255\000\000\005\255\236\255\236\255\236\255\236\255\
\128\255\128\255\128\255\128\255\254\254\000\000\000\000\000\000\
\005\255\000\000\000\000\236\255\000\000\000\000\000\000\000\000\
\000\000\000\000\104\255\075\255\000\000\023\255\000\000\128\255\
\236\254\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\027\000\000\000\000\000\000\000\003\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\208\255\000\000\000\000\000\000\000\000\
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
\000\000\250\255\248\255\228\255\002\000\024\000\036\000\006\000"

let yytablesize = 350
let yytable = "\028\000\
\008\000\026\000\003\000\032\000\037\000\034\000\078\000\001\000\
\029\000\030\000\031\000\063\000\064\000\065\000\066\000\033\000\
\079\000\056\000\057\000\039\000\071\000\058\000\059\000\074\000\
\035\000\047\000\025\000\069\000\070\000\060\000\077\000\082\000\
\083\000\044\000\108\000\004\000\005\000\067\000\047\000\084\000\
\007\000\008\000\009\000\072\000\073\000\075\000\076\000\068\000\
\113\000\114\000\040\000\012\000\041\000\042\000\043\000\099\000\
\045\000\048\000\013\000\050\000\035\000\049\000\051\000\014\000\
\015\000\016\000\017\000\018\000\110\000\019\000\036\000\021\000\
\032\000\032\000\032\000\081\000\047\000\047\000\052\000\032\000\
\032\000\032\000\104\000\105\000\106\000\107\000\100\000\101\000\
\102\000\103\000\003\000\004\000\005\000\006\000\053\000\054\000\
\007\000\008\000\009\000\010\000\011\000\111\000\089\000\090\000\
\085\000\115\000\086\000\012\000\087\000\088\000\091\000\092\000\
\096\000\097\000\013\000\112\000\109\000\098\000\116\000\014\000\
\015\000\016\000\017\000\018\000\000\000\019\000\020\000\021\000\
\003\000\004\000\005\000\006\000\000\000\000\000\007\000\008\000\
\009\000\010\000\011\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\000\000\000\000\000\000\000\000\014\000\015\000\016\000\
\017\000\018\000\000\000\019\000\027\000\021\000\004\000\005\000\
\000\000\000\000\000\000\007\000\008\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\012\000\004\000\
\005\000\000\000\000\000\000\000\000\000\013\000\000\000\000\000\
\000\000\000\000\014\000\015\000\016\000\017\000\018\000\012\000\
\019\000\027\000\021\000\000\000\000\000\046\000\013\000\093\000\
\094\000\025\000\025\000\000\000\004\000\005\000\000\000\000\000\
\000\000\019\000\027\000\021\000\000\000\000\000\000\000\000\000\
\000\000\025\000\000\000\025\000\012\000\000\000\000\000\025\000\
\025\000\025\000\046\000\013\000\055\000\004\000\005\000\000\000\
\000\000\000\000\000\000\025\000\025\000\025\000\019\000\027\000\
\021\000\000\000\000\000\000\000\000\000\012\000\000\000\000\000\
\000\000\000\000\008\000\008\000\013\000\008\000\008\000\003\000\
\003\000\000\000\000\000\000\000\008\000\000\000\003\000\019\000\
\027\000\021\000\008\000\008\000\000\000\003\000\000\000\000\000\
\008\000\008\000\008\000\008\000\025\000\025\000\000\000\004\000\
\005\000\000\000\000\000\000\000\008\000\008\000\008\000\000\000\
\000\000\000\000\000\000\000\000\025\000\000\000\025\000\012\000\
\000\000\000\000\025\000\025\000\000\000\046\000\013\000\000\000\
\095\000\004\000\005\000\000\000\000\000\000\000\025\000\025\000\
\025\000\019\000\027\000\021\000\000\000\000\000\000\000\000\000\
\000\000\012\000\000\000\000\000\000\000\000\000\000\000\046\000\
\013\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\027\000\021\000"

let yycheck = "\006\000\
\000\000\037\001\000\000\037\001\013\000\012\000\027\001\001\000\
\007\000\008\000\009\000\040\000\041\000\042\000\043\000\037\001\
\037\001\013\001\014\001\029\001\049\000\017\001\018\001\052\000\
\027\001\024\000\000\000\036\001\037\001\025\001\059\000\015\001\
\016\001\021\001\037\001\002\001\003\001\044\000\037\000\023\001\
\007\001\008\001\009\001\050\000\051\000\054\000\055\000\046\000\
\026\001\027\001\029\001\018\001\029\001\029\001\029\001\084\000\
\000\000\020\001\025\001\005\001\027\001\022\001\021\001\030\001\
\031\001\032\001\033\001\034\001\097\000\036\001\037\001\038\001\
\019\001\020\001\021\001\028\001\075\000\076\000\022\001\026\001\
\027\001\028\001\089\000\090\000\091\000\092\000\085\000\086\000\
\087\000\088\000\001\001\002\001\003\001\004\001\019\001\021\001\
\007\001\008\001\009\001\010\001\011\001\100\000\020\001\006\001\
\028\001\112\000\028\001\018\001\028\001\028\001\012\001\021\001\
\019\001\022\001\025\001\012\001\093\000\082\000\113\000\030\001\
\031\001\032\001\033\001\034\001\255\255\036\001\037\001\038\001\
\001\001\002\001\003\001\004\001\255\255\255\255\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\255\255\255\255\255\255\
\255\255\018\001\255\255\255\255\255\255\255\255\255\255\255\255\
\025\001\255\255\255\255\255\255\255\255\030\001\031\001\032\001\
\033\001\034\001\255\255\036\001\037\001\038\001\002\001\003\001\
\255\255\255\255\255\255\007\001\008\001\009\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\018\001\002\001\
\003\001\255\255\255\255\255\255\255\255\025\001\255\255\255\255\
\255\255\255\255\030\001\031\001\032\001\033\001\034\001\018\001\
\036\001\037\001\038\001\255\255\255\255\024\001\025\001\026\001\
\027\001\002\001\003\001\255\255\002\001\003\001\255\255\255\255\
\255\255\036\001\037\001\038\001\255\255\255\255\255\255\255\255\
\255\255\018\001\255\255\020\001\018\001\255\255\255\255\024\001\
\025\001\026\001\024\001\025\001\026\001\002\001\003\001\255\255\
\255\255\255\255\255\255\036\001\037\001\038\001\036\001\037\001\
\038\001\255\255\255\255\255\255\255\255\018\001\255\255\255\255\
\255\255\255\255\002\001\003\001\025\001\005\001\006\001\005\001\
\006\001\255\255\255\255\255\255\012\001\255\255\012\001\036\001\
\037\001\038\001\018\001\019\001\255\255\019\001\255\255\255\255\
\024\001\025\001\026\001\027\001\002\001\003\001\255\255\002\001\
\003\001\255\255\255\255\255\255\036\001\037\001\038\001\255\255\
\255\255\255\255\255\255\255\255\018\001\255\255\020\001\018\001\
\255\255\255\255\024\001\025\001\255\255\024\001\025\001\255\255\
\027\001\002\001\003\001\255\255\255\255\255\255\036\001\037\001\
\038\001\036\001\037\001\038\001\255\255\255\255\255\255\255\255\
\255\255\018\001\255\255\255\255\255\255\255\255\255\255\024\001\
\025\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\036\001\037\001\038\001"

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
# 311 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 57 "parser.mly"
        ( Bind (_1, _3) )
# 319 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 61 "parser.mly"
      ( _1 )
# 326 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Lambda.term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 63 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 335 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 65 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 344 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 67 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 353 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 69 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2,_4,_6)), _8) )
# 363 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 73 "parser.mly"
      ( _1 )
# 370 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 75 "parser.mly"
      ( TmSucc _2 )
# 377 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 77 "parser.mly"
      ( TmPred _2 )
# 384 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 79 "parser.mly"
      ( TmIsZero _2 )
# 391 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 81 "parser.mly"
      ( TmApp (_1, _2) )
# 399 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 83 "parser.mly"
      ( TmProj (_1, (string_of_int _3)))
# 407 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
      ( TmProj (_1, _3))
# 415 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'appTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 87 "parser.mly"
      ( TmConcat (_1, _3))
# 423 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 89 "parser.mly"
      ( TmNil _3 )
# 430 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 91 "parser.mly"
      ( TmCons (_3, _5, _6) )
# 439 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 93 "parser.mly"
      ( TmIsNil (_3, _5))
# 447 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 95 "parser.mly"
      ( TmHead (_3, _5))
# 455 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 97 "parser.mly"
      ( TmTail (_3, _5))
# 463 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lambda.term) in
    Obj.repr(
# 101 "parser.mly"
      ( _2 )
# 470 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'appTerm) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    Obj.repr(
# 103 "parser.mly"
      ( TmPair (_2, _4) )
# 478 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
      ( TmTrue )
# 484 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser.mly"
      ( TmFalse )
# 490 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
      ( TmVar _1 )
# 497 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 111 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 507 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "parser.mly"
    ( TmString _1 )
# 514 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'recordTM) in
    Obj.repr(
# 118 "parser.mly"
    ( TmRecord _2 )
# 521 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
              ( [] )
# 527 "parser.ml"
               : 'recordTM))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    Obj.repr(
# 122 "parser.mly"
                                 ( [(_1,_3)] )
# 535 "parser.ml"
               : 'recordTM))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'appTerm) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'recordTM) in
    Obj.repr(
# 123 "parser.mly"
                                       ( ((_1,_3)::(_5)) )
# 544 "parser.ml"
               : 'recordTM))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 127 "parser.mly"
      ( _1 )
# 551 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 129 "parser.mly"
      ( TyArr (_1, _3) )
# 559 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 131 "parser.mly"
      ( TyPair (_1, _3) )
# 567 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTy) in
    Obj.repr(
# 133 "parser.mly"
      ( TyList _1 )
# 574 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 137 "parser.mly"
      ( _2 )
# 581 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "parser.mly"
      ( TyBool )
# 587 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
      ( TyNat )
# 593 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
      ( TyString )
# 599 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'recordTY) in
    Obj.repr(
# 145 "parser.mly"
      ( TyRecord _2 )
# 606 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 148 "parser.mly"
              ( [] )
# 612 "parser.ml"
               : 'recordTY))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 149 "parser.mly"
                               ( [(_1,_3)] )
# 620 "parser.ml"
               : 'recordTY))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'recordTY) in
    Obj.repr(
# 150 "parser.mly"
                                     ( ((_1,_3)::(_5)) )
# 629 "parser.ml"
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
