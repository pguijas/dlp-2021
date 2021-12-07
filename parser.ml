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
  | LBRACKET
  | COMMA
  | RBRACKET
  | QUOTE
  | EOF
  | INTV of (int)
  | STRINGV of (string)
  | TSTRING of (string)

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 38 "parser.ml"
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
  279 (* LBRACKET *);
  280 (* COMMA *);
  281 (* RBRACKET *);
  282 (* QUOTE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  283 (* INTV *);
  284 (* STRINGV *);
  285 (* TSTRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\004\000\004\000\004\000\
\006\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\002\000\003\000\003\000\005\000\001\000\
\001\000\001\000\001\000\003\000\003\000\001\000\003\000\003\000\
\003\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\016\000\017\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\019\000\000\000\
\029\000\000\000\000\000\000\000\000\000\018\000\000\000\009\000\
\010\000\011\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\012\000\000\000\000\000\000\000\000\000\000\000\
\014\000\000\000\020\000\021\000\002\000\013\000\026\000\027\000\
\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\
\025\000\005\000\024\000\023\000\004\000\006\000\000\000\000\000\
\007\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\051\000\020\000\052\000"

let yysindex = "\006\000\
\060\255\000\000\247\254\000\000\000\000\088\255\128\255\128\255\
\128\255\252\254\254\254\088\255\013\255\239\254\000\000\003\255\
\000\000\029\000\128\255\012\255\014\255\000\000\032\255\000\000\
\000\000\000\000\024\255\026\255\016\255\100\255\027\255\028\255\
\088\255\000\000\000\000\031\255\029\255\088\255\088\255\029\255\
\000\000\013\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\255\040\255\251\254\054\255\053\255\052\255\116\255\
\055\255\088\255\029\255\029\255\088\255\088\255\088\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\062\255\088\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\005\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\030\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\250\255\252\255\224\255\251\255\016\000"

let yytablesize = 297
let yytable = "\023\000\
\008\000\024\000\025\000\026\000\003\000\029\000\001\000\055\000\
\030\000\059\000\031\000\032\000\018\000\035\000\004\000\005\000\
\060\000\057\000\021\000\007\000\008\000\009\000\033\000\027\000\
\035\000\028\000\045\000\068\000\034\000\012\000\036\000\053\000\
\054\000\041\000\037\000\013\000\038\000\056\000\014\000\015\000\
\022\000\047\000\048\000\039\000\049\000\050\000\040\000\022\000\
\022\000\022\000\035\000\066\000\043\000\044\000\069\000\070\000\
\071\000\046\000\058\000\061\000\003\000\004\000\005\000\006\000\
\062\000\073\000\007\000\008\000\009\000\010\000\011\000\063\000\
\065\000\072\000\067\000\000\000\012\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\000\000\014\000\015\000\016\000\
\003\000\004\000\005\000\006\000\000\000\000\000\007\000\008\000\
\009\000\010\000\011\000\000\000\000\000\004\000\005\000\000\000\
\012\000\000\000\000\000\000\000\000\000\000\000\013\000\000\000\
\000\000\014\000\015\000\022\000\012\000\004\000\005\000\000\000\
\000\000\000\000\013\000\042\000\000\000\014\000\015\000\022\000\
\000\000\004\000\005\000\000\000\012\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\064\000\014\000\015\000\022\000\
\012\000\000\000\000\000\000\000\000\000\000\000\013\000\000\000\
\000\000\014\000\015\000\022\000\000\000\000\000\000\000\000\000\
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
\000\000\003\000\003\000\000\000\008\000\000\000\018\000\018\000\
\003\000\008\000\008\000\000\000\000\000\000\000\003\000\008\000\
\008\000\008\000\008\000\008\000\008\000\018\000\000\000\018\000\
\000\000\000\000\000\000\018\000\000\000\000\000\018\000\018\000\
\018\000"

let yycheck = "\006\000\
\000\000\007\000\008\000\009\000\000\000\012\000\001\000\040\000\
\013\000\015\001\028\001\029\001\000\000\019\000\002\001\003\001\
\022\001\050\000\028\001\007\001\008\001\009\001\020\001\028\001\
\030\000\028\001\033\000\060\000\000\000\017\001\019\001\038\000\
\039\000\018\001\021\001\023\001\005\001\042\000\026\001\027\001\
\028\001\013\001\014\001\020\001\016\001\017\001\021\001\018\001\
\019\001\020\001\056\000\058\000\026\001\026\001\061\000\062\000\
\063\000\027\001\019\001\006\001\001\001\002\001\003\001\004\001\
\012\001\072\000\007\001\008\001\009\001\010\001\011\001\020\001\
\018\001\012\001\059\000\255\255\017\001\255\255\255\255\255\255\
\255\255\255\255\023\001\255\255\255\255\026\001\027\001\028\001\
\001\001\002\001\003\001\004\001\255\255\255\255\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\002\001\003\001\255\255\
\017\001\255\255\255\255\255\255\255\255\255\255\023\001\255\255\
\255\255\026\001\027\001\028\001\017\001\002\001\003\001\255\255\
\255\255\255\255\023\001\024\001\255\255\026\001\027\001\028\001\
\255\255\002\001\003\001\255\255\017\001\255\255\255\255\255\255\
\255\255\255\255\023\001\255\255\025\001\026\001\027\001\028\001\
\017\001\255\255\255\255\255\255\255\255\255\255\023\001\255\255\
\255\255\026\001\027\001\028\001\255\255\255\255\255\255\255\255\
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
\012\001\017\001\018\001\255\255\255\255\255\255\018\001\023\001\
\024\001\025\001\026\001\027\001\028\001\017\001\255\255\019\001\
\255\255\255\255\255\255\023\001\255\255\255\255\026\001\027\001\
\028\001"

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
  LBRACKET\000\
  COMMA\000\
  RBRACKET\000\
  QUOTE\000\
  EOF\000\
  "

let yynames_block = "\
  INTV\000\
  STRINGV\000\
  TSTRING\000\
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
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 94 "parser.mly"
    ( TmString _2 )
# 400 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 96 "parser.mly"
    ( TmString _2 )
# 407 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 100 "parser.mly"
      ( _1 )
# 414 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 102 "parser.mly"
      ( TyArr (_1, _3) )
# 422 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 104 "parser.mly"
      ( TyPair (_1, _3) )
# 430 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 108 "parser.mly"
      ( _2 )
# 437 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
      ( TyBool )
# 443 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
      ( TyNat )
# 449 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
      ( TyString )
# 455 "parser.ml"
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
