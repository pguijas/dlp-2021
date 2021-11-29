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
# 34 "parser.ml"
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
  271 (* LPAREN *);
  272 (* RPAREN *);
  273 (* DOT *);
  274 (* EQ *);
  275 (* COLON *);
  276 (* ARROW *);
  277 (* LBRACKET *);
  278 (* COMMA *);
  279 (* RBRACKET *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  280 (* INTV *);
  281 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\005\000\005\000\
\005\000\005\000\005\000\004\000\004\000\006\000\006\000\006\000\
\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\002\000\007\000\005\000\003\000\001\000\
\001\000\001\000\001\000\001\000\003\000\003\000\001\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\016\000\017\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\000\000\025\000\
\000\000\000\000\008\000\000\000\018\000\000\000\009\000\010\000\
\011\000\000\000\000\000\000\000\000\000\000\000\001\000\012\000\
\000\000\000\000\000\000\000\000\015\000\000\000\002\000\023\000\
\024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\022\000\
\005\000\021\000\004\000\006\000\000\000\000\000\000\000\013\000\
\007\000"

let yydgoto = "\002\000\
\016\000\017\000\018\000\043\000\019\000\044\000"

let yysindex = "\004\000\
\032\255\000\000\235\254\000\000\000\000\059\255\013\255\013\255\
\013\255\238\254\244\254\059\255\070\255\000\000\252\254\000\000\
\017\000\013\255\000\000\000\255\000\000\015\255\000\000\000\000\
\000\000\005\255\008\255\028\255\084\255\059\255\000\000\000\000\
\087\255\059\255\059\255\087\255\000\000\070\255\000\000\000\000\
\000\000\087\255\007\255\009\255\039\255\040\255\036\255\073\255\
\042\255\059\255\087\255\059\255\059\255\059\255\038\255\000\000\
\000\000\000\000\000\000\000\000\052\255\041\255\059\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\000\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\094\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\252\255\243\255\223\255\003\000\000\000"

let yytablesize = 294
let yytable = "\029\000\
\014\000\022\000\047\000\020\000\001\000\018\000\026\000\028\000\
\049\000\023\000\024\000\025\000\027\000\030\000\004\000\005\000\
\031\000\058\000\033\000\034\000\032\000\003\000\035\000\050\000\
\048\000\039\000\036\000\012\000\051\000\045\000\046\000\032\000\
\003\000\004\000\005\000\006\000\014\000\021\000\007\000\008\000\
\009\000\010\000\011\000\037\000\052\000\057\000\012\000\059\000\
\060\000\061\000\032\000\053\000\013\000\054\000\062\000\014\000\
\015\000\056\000\065\000\003\000\004\000\005\000\006\000\063\000\
\064\000\007\000\008\000\009\000\010\000\011\000\000\000\004\000\
\005\000\012\000\004\000\005\000\007\000\008\000\009\000\013\000\
\000\000\000\000\014\000\021\000\012\000\004\000\005\000\012\000\
\000\000\000\000\013\000\000\000\000\000\014\000\021\000\055\000\
\014\000\021\000\012\000\040\000\041\000\042\000\000\000\000\000\
\000\000\038\000\000\000\014\000\021\000\020\000\020\000\020\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\014\000\000\000\014\000\014\000\018\000\
\018\000\000\000\000\000\000\000\014\000\000\000\000\000\014\000\
\014\000\000\000\000\000\000\000\018\000\000\000\014\000\014\000\
\014\000\014\000\003\000\003\000\000\000\018\000\018\000\000\000\
\000\000\003\000\000\000\000\000\000\000\003\000"

let yycheck = "\013\000\
\000\000\006\000\036\000\025\001\001\000\000\000\025\001\012\000\
\042\000\007\000\008\000\009\000\025\001\018\001\002\001\003\001\
\000\000\051\000\019\001\005\001\018\000\000\000\018\001\017\001\
\038\000\030\000\019\001\015\001\020\001\034\000\035\000\029\000\
\001\001\002\001\003\001\004\001\024\001\025\001\007\001\008\001\
\009\001\010\001\011\001\016\001\006\001\050\000\015\001\052\000\
\053\000\054\000\048\000\012\001\021\001\018\001\017\001\024\001\
\025\001\016\001\063\000\001\001\002\001\003\001\004\001\012\001\
\024\001\007\001\008\001\009\001\010\001\011\001\255\255\002\001\
\003\001\015\001\002\001\003\001\007\001\008\001\009\001\021\001\
\255\255\255\255\024\001\025\001\015\001\002\001\003\001\015\001\
\255\255\255\255\021\001\255\255\255\255\024\001\025\001\023\001\
\024\001\025\001\015\001\013\001\014\001\015\001\255\255\255\255\
\255\255\022\001\255\255\024\001\025\001\016\001\017\001\018\001\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\005\001\006\001\002\001\
\003\001\255\255\255\255\255\255\012\001\255\255\255\255\015\001\
\016\001\255\255\255\255\255\255\015\001\255\255\022\001\023\001\
\024\001\025\001\005\001\006\001\255\255\024\001\025\001\255\255\
\255\255\012\001\255\255\255\255\255\255\016\001"

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
# 43 "parser.mly"
      ( "",_1 )
# 236 "parser.ml"
               : string * Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 45 "parser.mly"
      ( _1,_3 )
# 244 "parser.ml"
               : string * Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 50 "parser.mly"
      ( _1 )
# 251 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Lambda.term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 52 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 260 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 54 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 269 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 56 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 278 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Lambda.term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Lambda.term) in
    Obj.repr(
# 58 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2,_4,_6)), _8) )
# 288 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 63 "parser.mly"
      ( _1 )
# 295 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 65 "parser.mly"
      ( TmSucc _2 )
# 302 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 67 "parser.mly"
      ( TmPred _2 )
# 309 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 69 "parser.mly"
      ( TmIsZero _2 )
# 316 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 71 "parser.mly"
      ( TmApp (_1, _2) )
# 324 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'appTerm) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'appTerm) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 73 "parser.mly"
      ( TmProj (TmPair (_2, _4), _7) )
# 333 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'appTerm) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    Obj.repr(
# 75 "parser.mly"
      ( TmPair (_2, _4) )
# 341 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lambda.term) in
    Obj.repr(
# 79 "parser.mly"
      ( _2 )
# 348 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
      ( TmTrue )
# 354 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
      ( TmFalse )
# 360 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
      ( TmVar _1 )
# 367 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 87 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 377 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 94 "parser.mly"
      ( _1 )
# 384 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 96 "parser.mly"
      ( TyArr (_1, _3) )
# 392 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 100 "parser.mly"
      ( _2 )
# 399 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
      ( TyBool )
# 405 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
      ( TyNat )
# 411 "parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : string * Lambda.term)
