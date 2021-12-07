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
  | QUOTE
  | EOF
  | INTV of (int)
  | STRINGV of (string)
  | STR_VAR of (string)

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 39 "parser.ml"
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
  283 (* QUOTE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  284 (* INTV *);
  285 (* STRINGV *);
  286 (* STR_VAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\004\000\004\000\004\000\
\006\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\002\000\003\000\003\000\003\000\005\000\
\001\000\001\000\001\000\001\000\003\000\001\000\003\000\003\000\
\003\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\017\000\018\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000\000\000\
\029\000\000\000\000\000\000\000\000\000\019\000\000\000\009\000\
\010\000\011\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\012\000\000\000\000\000\000\000\000\000\000\000\000\000\
\015\000\000\000\021\000\002\000\013\000\014\000\026\000\027\000\
\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\
\025\000\005\000\024\000\023\000\004\000\006\000\000\000\000\000\
\007\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\051\000\020\000\052\000"

let yysindex = "\011\000\
\037\255\000\000\243\254\000\000\000\000\067\255\116\255\116\255\
\116\255\250\254\251\254\067\255\079\255\255\254\000\000\009\255\
\000\000\035\000\116\255\011\255\015\255\000\000\032\255\000\000\
\000\000\000\000\022\255\029\255\025\255\087\255\026\255\067\255\
\000\000\000\000\030\255\116\255\005\255\067\255\067\255\005\255\
\000\000\079\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\005\255\040\255\249\254\054\255\050\255\043\255\096\255\
\055\255\067\255\005\255\005\255\067\255\067\255\067\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\060\255\067\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\005\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\247\254\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\250\255\007\000\223\255\251\255\020\000"

let yytablesize = 299
let yytable = "\023\000\
\008\000\024\000\025\000\026\000\003\000\029\000\055\000\059\000\
\022\000\022\000\022\000\001\000\019\000\034\000\060\000\021\000\
\057\000\047\000\048\000\030\000\049\000\050\000\027\000\028\000\
\034\000\044\000\068\000\031\000\032\000\035\000\046\000\053\000\
\054\000\036\000\033\000\037\000\038\000\003\000\004\000\005\000\
\006\000\039\000\041\000\007\000\008\000\009\000\010\000\011\000\
\056\000\040\000\034\000\066\000\043\000\012\000\069\000\070\000\
\071\000\045\000\058\000\061\000\013\000\062\000\063\000\014\000\
\015\000\073\000\016\000\003\000\004\000\005\000\006\000\072\000\
\065\000\007\000\008\000\009\000\010\000\011\000\067\000\000\000\
\004\000\005\000\000\000\012\000\000\000\007\000\008\000\009\000\
\004\000\005\000\013\000\000\000\000\000\014\000\015\000\012\000\
\022\000\004\000\005\000\000\000\000\000\000\000\013\000\012\000\
\000\000\014\000\015\000\000\000\022\000\000\000\013\000\042\000\
\012\000\014\000\015\000\000\000\022\000\004\000\005\000\013\000\
\000\000\064\000\014\000\015\000\000\000\022\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\014\000\015\000\
\000\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\008\000\008\000\000\000\008\000\008\000\000\000\
\000\000\003\000\003\000\000\000\008\000\000\000\019\000\019\000\
\003\000\008\000\008\000\000\000\000\000\000\000\003\000\000\000\
\008\000\008\000\008\000\008\000\008\000\019\000\008\000\019\000\
\000\000\000\000\000\000\019\000\019\000\000\000\000\000\019\000\
\019\000\000\000\019\000"

let yycheck = "\006\000\
\000\000\007\000\008\000\009\000\000\000\012\000\040\000\015\001\
\018\001\019\001\020\001\001\000\000\000\019\000\022\001\029\001\
\050\000\013\001\014\001\013\000\016\001\017\001\029\001\029\001\
\030\000\032\000\060\000\029\001\020\001\019\001\036\000\038\000\
\039\000\023\001\000\000\021\001\005\001\001\001\002\001\003\001\
\004\001\020\001\018\001\007\001\008\001\009\001\010\001\011\001\
\042\000\021\001\056\000\058\000\027\001\017\001\061\000\062\000\
\063\000\028\001\019\001\006\001\024\001\012\001\020\001\027\001\
\028\001\072\000\030\001\001\001\002\001\003\001\004\001\012\001\
\018\001\007\001\008\001\009\001\010\001\011\001\059\000\255\255\
\002\001\003\001\255\255\017\001\255\255\007\001\008\001\009\001\
\002\001\003\001\024\001\255\255\255\255\027\001\028\001\017\001\
\030\001\002\001\003\001\255\255\255\255\255\255\024\001\017\001\
\255\255\027\001\028\001\255\255\030\001\255\255\024\001\025\001\
\017\001\027\001\028\001\255\255\030\001\002\001\003\001\024\001\
\255\255\026\001\027\001\028\001\255\255\030\001\255\255\255\255\
\255\255\255\255\255\255\255\255\017\001\255\255\255\255\255\255\
\255\255\255\255\255\255\024\001\255\255\255\255\027\001\028\001\
\255\255\030\001\255\255\255\255\255\255\255\255\255\255\255\255\
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
\255\255\255\255\002\001\003\001\255\255\005\001\006\001\255\255\
\255\255\005\001\006\001\255\255\012\001\255\255\002\001\003\001\
\012\001\017\001\018\001\255\255\255\255\255\255\018\001\255\255\
\024\001\025\001\026\001\027\001\028\001\017\001\030\001\019\001\
\255\255\255\255\255\255\023\001\024\001\255\255\255\255\027\001\
\028\001\255\255\030\001"

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
  QUOTE\000\
  EOF\000\
  "

let yynames_block = "\
  INTV\000\
  STRINGV\000\
  STR_VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lambda.term) in
    Obj.repr(
# 48 "parser.mly"
        ( Eval _1 )
# 256 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 50 "parser.mly"
        ( Bind (_1, _3) )
# 264 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 54 "parser.mly"
      ( _1 )
# 271 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Lambda.term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 56 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 280 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 58 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 289 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 60 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 298 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 62 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2,_4,_6)), _8) )
# 308 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 66 "parser.mly"
      ( _1 )
# 315 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 68 "parser.mly"
      ( TmSucc _2 )
# 322 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 70 "parser.mly"
      ( TmPred _2 )
# 329 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 72 "parser.mly"
      ( TmIsZero _2 )
# 336 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 74 "parser.mly"
      ( TmApp (_1, _2) )
# 344 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 76 "parser.mly"
      ( TmProj (_1, _3))
# 352 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 78 "parser.mly"
      ( TmConcat (_1, _3) )
# 360 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lambda.term) in
    Obj.repr(
# 82 "parser.mly"
      ( _2 )
# 367 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'appTerm) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    Obj.repr(
# 84 "parser.mly"
      ( TmPair (_2, _4) )
# 375 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
      ( TmTrue )
# 381 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
      ( TmFalse )
# 387 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parser.mly"
      ( TmVar _1 )
# 394 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 92 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 404 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 97 "parser.mly"
    ( TmString _2 )
# 411 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 101 "parser.mly"
      ( _1 )
# 418 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 103 "parser.mly"
      ( TyArr (_1, _3) )
# 426 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 105 "parser.mly"
      ( TyPair (_1, _3) )
# 434 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 109 "parser.mly"
      ( _2 )
# 441 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
      ( TyBool )
# 447 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
      ( TyNat )
# 453 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
      ( TyString )
# 459 "parser.ml"
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
