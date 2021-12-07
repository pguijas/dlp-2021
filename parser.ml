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
  | TSTR of (string)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Lambda;;
# 36 "parser.ml"
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
  283 (* TSTR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\004\000\004\000\004\000\006\000\
\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\002\000\003\000\003\000\005\000\001\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\016\000\017\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\000\000\020\000\
\027\000\000\000\000\000\000\000\000\000\018\000\000\000\009\000\
\010\000\011\000\000\000\000\000\000\000\000\000\000\000\001\000\
\012\000\000\000\000\000\000\000\000\000\000\000\014\000\000\000\
\002\000\013\000\025\000\026\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\024\000\005\000\023\000\022\000\004\000\
\006\000\000\000\000\000\007\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\046\000\020\000\047\000"

let yysindex = "\004\000\
\058\255\000\000\236\254\000\000\000\000\085\255\124\255\124\255\
\124\255\239\254\241\254\085\255\097\255\000\000\249\254\000\000\
\000\000\018\000\124\255\001\255\004\255\000\000\023\255\000\000\
\000\000\000\000\010\255\015\255\019\255\018\255\085\255\000\000\
\000\000\013\255\009\255\085\255\085\255\009\255\000\000\097\255\
\000\000\000\000\000\000\000\000\009\255\021\255\245\254\036\255\
\038\255\032\255\112\255\039\255\085\255\009\255\009\255\085\255\
\085\255\085\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\043\255\085\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\013\000\000\000\
\000\000\000\000\031\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\029\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\252\255\246\255\218\255\007\000\003\000"

let yytablesize = 304
let yytable = "\050\000\
\008\000\023\000\030\000\054\000\001\000\021\000\052\000\029\000\
\027\000\055\000\028\000\031\000\018\000\024\000\025\000\026\000\
\063\000\032\000\034\000\004\000\005\000\043\000\044\000\035\000\
\045\000\033\000\041\000\036\000\037\000\051\000\003\000\048\000\
\049\000\012\000\038\000\039\000\033\000\042\000\053\000\013\000\
\040\000\056\000\014\000\022\000\016\000\021\000\021\000\021\000\
\061\000\057\000\058\000\064\000\065\000\066\000\067\000\060\000\
\062\000\033\000\003\000\004\000\005\000\006\000\068\000\000\000\
\007\000\008\000\009\000\010\000\011\000\000\000\000\000\000\000\
\000\000\012\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\014\000\015\000\016\000\003\000\004\000\005\000\
\006\000\000\000\000\000\007\000\008\000\009\000\010\000\011\000\
\000\000\000\000\004\000\005\000\012\000\000\000\000\000\007\000\
\008\000\009\000\013\000\000\000\000\000\014\000\022\000\016\000\
\012\000\004\000\005\000\000\000\000\000\000\000\013\000\000\000\
\000\000\014\000\022\000\016\000\000\000\004\000\005\000\012\000\
\000\000\000\000\000\000\000\000\000\000\013\000\000\000\059\000\
\014\000\022\000\016\000\012\000\000\000\000\000\000\000\000\000\
\000\000\013\000\000\000\000\000\014\000\022\000\016\000\000\000\
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
\000\000\000\000\000\000\000\000\008\000\000\000\018\000\018\000\
\008\000\008\000\000\000\000\000\000\000\000\000\008\000\008\000\
\008\000\008\000\008\000\008\000\018\000\000\000\018\000\000\000\
\000\000\000\000\018\000\003\000\003\000\018\000\018\000\018\000\
\000\000\000\000\003\000\000\000\000\000\000\000\000\000\003\000"

let yycheck = "\038\000\
\000\000\006\000\013\000\015\001\001\000\026\001\045\000\012\000\
\026\001\021\001\026\001\019\001\000\000\007\000\008\000\009\000\
\055\000\000\000\018\001\002\001\003\001\013\001\014\001\020\001\
\016\001\019\000\031\000\005\001\019\001\040\000\000\000\036\000\
\037\000\016\001\020\001\017\001\030\000\025\001\018\001\022\001\
\023\001\006\001\025\001\026\001\027\001\017\001\018\001\019\001\
\053\000\012\001\019\001\056\000\057\000\058\000\012\001\017\001\
\054\000\051\000\001\001\002\001\003\001\004\001\067\000\255\255\
\007\001\008\001\009\001\010\001\011\001\255\255\255\255\255\255\
\255\255\016\001\255\255\255\255\255\255\255\255\255\255\022\001\
\255\255\255\255\025\001\026\001\027\001\001\001\002\001\003\001\
\004\001\255\255\255\255\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\002\001\003\001\016\001\255\255\255\255\007\001\
\008\001\009\001\022\001\255\255\255\255\025\001\026\001\027\001\
\016\001\002\001\003\001\255\255\255\255\255\255\022\001\255\255\
\255\255\025\001\026\001\027\001\255\255\002\001\003\001\016\001\
\255\255\255\255\255\255\255\255\255\255\022\001\255\255\024\001\
\025\001\026\001\027\001\016\001\255\255\255\255\255\255\255\255\
\255\255\022\001\255\255\255\255\025\001\026\001\027\001\255\255\
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
\255\255\255\255\255\255\255\255\012\001\255\255\002\001\003\001\
\016\001\017\001\255\255\255\255\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\027\001\016\001\255\255\018\001\255\255\
\255\255\255\255\022\001\005\001\006\001\025\001\026\001\027\001\
\255\255\255\255\012\001\255\255\255\255\255\255\255\255\017\001"

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
  TSTR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lambda.term) in
    Obj.repr(
# 44 "parser.mly"
        ( Eval _1 )
# 244 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 46 "parser.mly"
        ( Bind (_1, _3) )
# 252 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 50 "parser.mly"
      ( _1 )
# 259 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Lambda.term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 52 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 268 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 54 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 277 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 56 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 286 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 58 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2,_4,_6)), _8) )
# 296 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 62 "parser.mly"
      ( _1 )
# 303 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 64 "parser.mly"
      ( TmSucc _2 )
# 310 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 66 "parser.mly"
      ( TmPred _2 )
# 317 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 68 "parser.mly"
      ( TmIsZero _2 )
# 324 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 70 "parser.mly"
      ( TmApp (_1, _2) )
# 332 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 72 "parser.mly"
      ( TmProj (_1, _3) (* esto daba conflictos: term -> atomicTerm *))
# 340 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lambda.term) in
    Obj.repr(
# 76 "parser.mly"
      ( _2 )
# 347 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'appTerm) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    Obj.repr(
# 78 "parser.mly"
      ( TmPair (_2, _4) )
# 355 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
      ( TmTrue )
# 361 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
      ( TmFalse )
# 367 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
      ( TmVar _1 )
# 374 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 86 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 384 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "parser.mly"
      ( TmString _1 )
# 391 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 95 "parser.mly"
      ( _1 )
# 398 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 97 "parser.mly"
      ( TyArr (_1, _3) )
# 406 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 99 "parser.mly"
      ( TyPair (_1, _3) )
# 414 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 103 "parser.mly"
      ( _2 )
# 421 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
      ( TyBool )
# 427 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser.mly"
      ( TyNat )
# 433 "parser.ml"
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
