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
  | STRINGT of (string)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
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
  286 (* STRINGT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\004\000\004\000\004\000\006\000\
\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\002\000\003\000\003\000\005\000\001\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\016\000\017\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\000\000\020\000\
\028\000\000\000\000\000\000\000\000\000\018\000\000\000\009\000\
\010\000\011\000\000\000\000\000\000\000\000\000\000\000\001\000\
\012\000\000\000\000\000\000\000\000\000\000\000\014\000\000\000\
\002\000\013\000\025\000\026\000\027\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\015\000\024\000\005\000\023\000\022\000\
\004\000\006\000\000\000\000\000\007\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\047\000\020\000\048\000"

let yysindex = "\014\000\
\056\255\000\000\229\254\000\000\000\000\086\255\104\255\104\255\
\104\255\230\254\232\254\086\255\005\255\000\000\000\255\000\000\
\000\000\023\000\104\255\007\255\015\255\000\000\023\255\000\000\
\000\000\000\000\020\255\028\255\029\255\053\255\086\255\000\000\
\000\000\022\255\025\255\086\255\086\255\025\255\000\000\005\255\
\000\000\000\000\000\000\000\000\000\000\025\255\042\255\002\255\
\062\255\057\255\051\255\096\255\054\255\086\255\025\255\025\255\
\086\255\086\255\086\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\063\255\086\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\018\000\000\000\
\000\000\000\000\004\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\026\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\003\000\237\255\002\000\019\000"

let yytablesize = 304
let yytable = "\023\000\
\008\000\021\000\027\000\003\000\028\000\029\000\004\000\005\000\
\024\000\025\000\026\000\007\000\008\000\009\000\001\000\030\000\
\055\000\018\000\051\000\031\000\033\000\012\000\032\000\056\000\
\041\000\034\000\053\000\036\000\013\000\049\000\050\000\033\000\
\014\000\022\000\016\000\035\000\064\000\043\000\044\000\037\000\
\045\000\046\000\052\000\021\000\021\000\021\000\039\000\062\000\
\038\000\042\000\065\000\066\000\067\000\033\000\004\000\005\000\
\003\000\004\000\005\000\006\000\054\000\069\000\007\000\008\000\
\009\000\010\000\011\000\057\000\058\000\012\000\059\000\061\000\
\012\000\063\000\068\000\000\000\013\000\040\000\000\000\013\000\
\014\000\022\000\016\000\014\000\015\000\016\000\003\000\004\000\
\005\000\006\000\000\000\000\000\007\000\008\000\009\000\010\000\
\011\000\004\000\005\000\000\000\000\000\000\000\012\000\000\000\
\000\000\004\000\005\000\000\000\000\000\013\000\000\000\000\000\
\012\000\014\000\022\000\016\000\000\000\000\000\000\000\013\000\
\012\000\060\000\000\000\014\000\022\000\016\000\000\000\013\000\
\000\000\000\000\000\000\014\000\022\000\016\000\000\000\000\000\
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
\000\000\000\000\008\000\008\000\000\000\008\000\008\000\000\000\
\003\000\003\000\000\000\000\000\008\000\000\000\000\000\003\000\
\000\000\008\000\008\000\018\000\018\000\003\000\000\000\000\000\
\008\000\008\000\008\000\000\000\008\000\008\000\008\000\000\000\
\000\000\000\000\018\000\000\000\018\000\000\000\000\000\000\000\
\000\000\018\000\000\000\000\000\000\000\018\000\018\000\018\000"

let yycheck = "\006\000\
\000\000\029\001\029\001\000\000\029\001\012\000\002\001\003\001\
\007\000\008\000\009\000\007\001\008\001\009\001\001\000\013\000\
\015\001\000\000\038\000\020\001\019\000\017\001\000\000\022\001\
\031\000\019\001\046\000\005\001\024\001\036\000\037\000\030\000\
\028\001\029\001\030\001\021\001\056\000\013\001\014\001\020\001\
\016\001\017\001\040\000\018\001\019\001\020\001\018\001\054\000\
\021\001\028\001\057\000\058\000\059\000\052\000\002\001\003\001\
\001\001\002\001\003\001\004\001\019\001\068\000\007\001\008\001\
\009\001\010\001\011\001\006\001\012\001\017\001\020\001\018\001\
\017\001\055\000\012\001\255\255\024\001\025\001\255\255\024\001\
\028\001\029\001\030\001\028\001\029\001\030\001\001\001\002\001\
\003\001\004\001\255\255\255\255\007\001\008\001\009\001\010\001\
\011\001\002\001\003\001\255\255\255\255\255\255\017\001\255\255\
\255\255\002\001\003\001\255\255\255\255\024\001\255\255\255\255\
\017\001\028\001\029\001\030\001\255\255\255\255\255\255\024\001\
\017\001\026\001\255\255\028\001\029\001\030\001\255\255\024\001\
\255\255\255\255\255\255\028\001\029\001\030\001\255\255\255\255\
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
\255\255\255\255\002\001\003\001\255\255\005\001\006\001\255\255\
\005\001\006\001\255\255\255\255\012\001\255\255\255\255\012\001\
\255\255\017\001\018\001\002\001\003\001\018\001\255\255\255\255\
\024\001\025\001\026\001\255\255\028\001\029\001\030\001\255\255\
\255\255\255\255\017\001\255\255\019\001\255\255\255\255\255\255\
\255\255\024\001\255\255\255\255\255\255\028\001\029\001\030\001"

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
  STRINGT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lambda.term) in
    Obj.repr(
# 47 "parser.mly"
        ( Eval _1 )
# 253 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 49 "parser.mly"
        ( Bind (_1, _3) )
# 261 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 53 "parser.mly"
      ( _1 )
# 268 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Lambda.term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 55 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 277 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 57 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 286 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 59 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 295 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 61 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2,_4,_6)), _8) )
# 305 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 65 "parser.mly"
      ( _1 )
# 312 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 67 "parser.mly"
      ( TmSucc _2 )
# 319 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 69 "parser.mly"
      ( TmPred _2 )
# 326 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 71 "parser.mly"
      ( TmIsZero _2 )
# 333 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 73 "parser.mly"
      ( TmApp (_1, _2) )
# 341 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 75 "parser.mly"
      ( TmProj (_1, _3) (* esto daba conflictos: term -> atomicTerm *))
# 349 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lambda.term) in
    Obj.repr(
# 79 "parser.mly"
      ( _2 )
# 356 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'appTerm) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    Obj.repr(
# 81 "parser.mly"
      ( TmPair (_2, _4) )
# 364 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
      ( TmTrue )
# 370 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
      ( TmFalse )
# 376 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
      ( TmVar _1 )
# 383 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 89 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 393 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parser.mly"
    ( TmString _1 )
# 400 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 98 "parser.mly"
      ( _1 )
# 407 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 100 "parser.mly"
      ( TyArr (_1, _3) )
# 415 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 102 "parser.mly"
      ( TyPair (_1, _3) )
# 423 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 106 "parser.mly"
      ( _2 )
# 430 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
      ( TyBool )
# 436 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
      ( TyNat )
# 442 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
      ( TyString )
# 448 "parser.ml"
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
