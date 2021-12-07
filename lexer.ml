# 2 "lexer.mll"
 
  open Parser;;
  exception Lexical_error;; 

# 7 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\222\255\223\255\004\000\036\000\227\255\038\000\040\000\
    \070\000\072\000\079\000\080\000\083\000\087\000\093\000\094\000\
    \096\000\106\000\109\000\110\000\117\000\120\000\124\000\123\000\
    \127\000\136\000\133\000\146\000\139\000\150\000\154\000\157\000\
    \168\000\160\000\169\000\176\000\200\000\210\000\202\000\212\000\
    \214\000\228\000\236\000\238\000\240\000\246\000\244\000\252\000\
    \003\001\012\001\022\001\025\001\029\001\026\001\052\001\043\001\
    \056\001\066\001\073\001\079\001\082\001\092\001\103\001\113\001\
    \114\001\131\001\125\001\127\001\140\001\146\001\148\001\149\001\
    \157\001\158\001\159\001\167\001";
  Lexing.lex_backtrk =
   "\030\000\255\255\255\255\030\000\029\000\255\255\027\000\026\000\
    \025\000\024\000\030\000\022\000\021\000\020\000\019\000\018\000\
    \017\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\002\000\030\000\000\000\030\000\030\000\030\000\
    \030\000\030\000\001\000\011\000\030\000\030\000\012\000\030\000\
    \030\000\030\000\003\000\030\000\006\000\030\000\030\000\030\000\
    \004\000\013\000\030\000\005\000\030\000\030\000\030\000\010\000\
    \030\000\030\000\007\000\030\000\030\000\008\000\030\000\030\000\
    \009\000\030\000\030\000\014\000\030\000\015\000\030\000\030\000\
    \030\000\030\000\016\000\023\000";
  Lexing.lex_default =
   "\003\000\000\000\000\000\003\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \028\000\000\000\005\000\000\000\000\000\000\000\255\255\001\000\
    \015\000\014\000\016\000\255\255\007\000\010\000\013\000\000\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\011\000\001\000\000\000\012\000\000\000\255\255\
    \000\000\000\000\019\000\000\000\000\000\000\000\255\255\000\000\
    \255\255\000\000\255\255\255\255\026\000\255\255\018\000\255\255\
    \000\000\000\000\000\000\017\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\009\000\255\255\
    \000\000\255\255\000\000\255\255\000\000\022\000\024\000\000\000\
    \255\255\023\000\255\255\000\000\027\000\255\255\000\000\255\255\
    \020\000\255\255\255\255\021\000\025\000\255\255\255\255\255\255\
    \000\000\255\255\255\255\008\000\000\000\006\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \000\000\000\000\255\255\255\255\255\255\075\000\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
    \255\255\000\000\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \000\000\000\000\255\255\255\255\000\000\255\255\068\000\255\255\
    \255\255\255\255\255\255\000\000\000\000\255\255\000\000\255\255\
    \255\255\000\000\000\000\255\255\000\000\065\000\070\000\000\000\
    \045\000\051\000\000\000\255\255\255\255\000\000\000\000\062\000\
    \056\000\049\000\255\255\255\255\255\255\059\000\050\000\255\255\
    \039\000\255\255\000\000\030\000\255\255\000\000\255\255\029\000\
    \255\255\255\255\040\000\255\255\000\000\255\255\000\000\032\000\
    \002\000\034\000\000\000\255\255\255\255\255\255\255\255\031\000\
    \000\000\000\000\035\000\255\255\033\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\000\000\255\255\255\255\255\255\
    \000\000\000\000\036\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\037\000\255\255\255\255\
    \000\000\255\255\000\000\255\255\000\000\038\000\000\000\255\255\
    \255\255\043\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255\000\000\255\255\255\255\
    \255\255\042\000\000\000\041\000\000\000\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\044\000\255\255\255\255\255\255\
    \255\255\048\000\046\000\000\000\255\255\000\000\255\255\047\000\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\053\000\255\255\255\255\255\255\000\000\000\000\
    \255\255\255\255\255\255\255\255\054\000\255\255\255\255\000\000\
    \052\000\000\000\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\000\000\000\000\255\255\000\000\255\255\
    \255\255\255\255\255\255\055\000\255\255\255\255\255\255\058\000\
    \255\255\255\255\255\255\057\000\255\255\255\255\255\255\000\000\
    \255\255\000\000\060\000\255\255\255\255\061\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\063\000\255\255\255\255\255\255\
    \255\255\000\000\255\255\000\000\255\255\064\000\255\255\000\000\
    \255\255\255\255\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\000\000\
    \000\000\067\000\000\000\000\000\255\255\000\000\255\255\000\000\
    \255\255\000\000\066\000\000\000\255\255\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\072\000\000\000\
    \069\000\000\000\000\000\255\255\000\000\074\000\071\000\000\000\
    \000\000\000\000\000\000\073\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\255\255\255\255\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\255\255\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\255\255\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    ";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\255\255\255\255\003\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\255\255\003\000\
    \255\255\255\255\000\000\255\255\255\255\255\255\004\000\255\255\
    \006\000\255\255\007\000\004\000\000\000\006\000\000\000\007\000\
    \255\255\255\255\255\255\000\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\000\000\004\000\
    \255\255\006\000\255\255\007\000\255\255\000\000\000\000\255\255\
    \008\000\000\000\009\000\255\255\000\000\008\000\255\255\009\000\
    \000\000\010\000\011\000\000\000\000\000\012\000\010\000\011\000\
    \255\255\013\000\012\000\000\000\255\255\000\000\013\000\014\000\
    \015\000\008\000\016\000\009\000\014\000\015\000\255\255\016\000\
    \255\255\255\255\010\000\011\000\017\000\010\000\012\000\018\000\
    \019\000\017\000\013\000\255\255\018\000\019\000\255\255\020\000\
    \014\000\015\000\021\000\016\000\020\000\023\000\022\000\021\000\
    \255\255\024\000\023\000\022\000\255\255\017\000\024\000\026\000\
    \018\000\019\000\025\000\255\255\026\000\028\000\255\255\025\000\
    \020\000\255\255\028\000\021\000\027\000\255\255\023\000\022\000\
    \029\000\027\000\024\000\255\255\030\000\029\000\255\255\031\000\
    \026\000\030\000\033\000\025\000\031\000\255\255\028\000\033\000\
    \255\255\255\255\032\000\034\000\255\255\027\000\018\000\032\000\
    \034\000\029\000\035\000\255\255\255\255\030\000\255\255\035\000\
    \031\000\255\255\255\255\033\000\255\255\019\000\017\000\255\255\
    \024\000\023\000\255\255\032\000\034\000\255\255\255\255\020\000\
    \022\000\023\000\036\000\035\000\038\000\021\000\023\000\036\000\
    \025\000\038\000\255\255\027\000\037\000\255\255\039\000\027\000\
    \040\000\037\000\025\000\039\000\255\255\040\000\255\255\031\000\
    \000\000\033\000\255\255\036\000\003\000\038\000\041\000\030\000\
    \255\255\255\255\029\000\041\000\032\000\037\000\042\000\039\000\
    \043\000\040\000\044\000\042\000\255\255\043\000\046\000\044\000\
    \045\000\255\255\255\255\046\000\255\255\045\000\047\000\041\000\
    \255\255\255\255\035\000\047\000\004\000\048\000\006\000\042\000\
    \007\000\043\000\048\000\044\000\255\255\036\000\049\000\046\000\
    \255\255\045\000\255\255\049\000\255\255\037\000\255\255\047\000\
    \050\000\039\000\255\255\051\000\053\000\050\000\048\000\052\000\
    \051\000\053\000\255\255\255\255\052\000\255\255\008\000\049\000\
    \009\000\041\000\255\255\040\000\255\255\055\000\255\255\010\000\
    \011\000\050\000\055\000\012\000\051\000\053\000\054\000\013\000\
    \052\000\255\255\056\000\054\000\043\000\014\000\015\000\056\000\
    \016\000\047\000\045\000\255\255\057\000\255\255\055\000\046\000\
    \255\255\057\000\017\000\058\000\255\255\018\000\019\000\054\000\
    \058\000\059\000\255\255\056\000\060\000\020\000\059\000\255\255\
    \021\000\060\000\255\255\023\000\022\000\057\000\061\000\024\000\
    \255\255\255\255\052\000\061\000\058\000\026\000\255\255\255\255\
    \025\000\062\000\059\000\028\000\053\000\060\000\062\000\255\255\
    \050\000\255\255\027\000\063\000\064\000\255\255\029\000\061\000\
    \063\000\064\000\030\000\255\255\255\255\031\000\255\255\066\000\
    \033\000\067\000\062\000\054\000\066\000\065\000\067\000\057\000\
    \032\000\034\000\065\000\056\000\063\000\064\000\068\000\255\255\
    \035\000\255\255\059\000\068\000\069\000\060\000\070\000\071\000\
    \066\000\069\000\067\000\070\000\071\000\255\255\065\000\072\000\
    \073\000\074\000\255\255\255\255\072\000\073\000\074\000\068\000\
    \036\000\075\000\038\000\255\255\062\000\069\000\075\000\070\000\
    \071\000\255\255\037\000\255\255\039\000\063\000\040\000\255\255\
    \072\000\073\000\074\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\075\000\255\255\041\000\255\255\255\255\255\255\
    \255\255\066\000\255\255\255\255\042\000\255\255\043\000\255\255\
    \044\000\255\255\065\000\255\255\046\000\255\255\045\000\255\255\
    \255\255\255\255\255\255\255\255\047\000\255\255\071\000\255\255\
    \068\000\255\255\255\255\048\000\255\255\073\000\070\000\255\255\
    \255\255\255\255\255\255\072\000\049\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\050\000\255\255\
    \255\255\051\000\053\000\255\255\255\255\052\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\055\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\054\000\255\255\255\255\255\255\
    \056\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\057\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\058\000\255\255\255\255\255\255\255\255\255\255\059\000\
    \255\255\255\255\060\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\061\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\062\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\063\000\064\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\066\000\255\255\067\000\
    \255\255\255\255\255\255\065\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\068\000\255\255\255\255\255\255\
    \255\255\255\255\069\000\255\255\070\000\071\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\072\000\073\000\074\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\075\000\
    ";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 8 "lexer.mll"
                ( token lexbuf )
# 237 "lexer.ml"

  | 1 ->
# 9 "lexer.mll"
                ( LAMBDA )
# 242 "lexer.ml"

  | 2 ->
# 10 "lexer.mll"
                ( LAMBDA )
# 247 "lexer.ml"

  | 3 ->
# 11 "lexer.mll"
                ( TRUE )
# 252 "lexer.ml"

  | 4 ->
# 12 "lexer.mll"
                ( FALSE )
# 257 "lexer.ml"

  | 5 ->
# 13 "lexer.mll"
                ( IF )
# 262 "lexer.ml"

  | 6 ->
# 14 "lexer.mll"
                ( THEN )
# 267 "lexer.ml"

  | 7 ->
# 15 "lexer.mll"
                ( ELSE )
# 272 "lexer.ml"

  | 8 ->
# 16 "lexer.mll"
                ( SUCC )
# 277 "lexer.ml"

  | 9 ->
# 17 "lexer.mll"
                ( PRED )
# 282 "lexer.ml"

  | 10 ->
# 18 "lexer.mll"
                ( ISZERO )
# 287 "lexer.ml"

  | 11 ->
# 19 "lexer.mll"
                ( LET )
# 292 "lexer.ml"

  | 12 ->
# 20 "lexer.mll"
                ( LETREC )
# 297 "lexer.ml"

  | 13 ->
# 21 "lexer.mll"
                ( IN )
# 302 "lexer.ml"

  | 14 ->
# 22 "lexer.mll"
                ( BOOL )
# 307 "lexer.ml"

  | 15 ->
# 23 "lexer.mll"
                ( NAT )
# 312 "lexer.ml"

  | 16 ->
# 24 "lexer.mll"
                ( STRING )
# 317 "lexer.ml"

  | 17 ->
# 25 "lexer.mll"
                ( TPAIR )
# 322 "lexer.ml"

  | 18 ->
# 26 "lexer.mll"
                ( LPAREN )
# 327 "lexer.ml"

  | 19 ->
# 27 "lexer.mll"
                ( RPAREN )
# 332 "lexer.ml"

  | 20 ->
# 28 "lexer.mll"
                ( DOT )
# 337 "lexer.ml"

  | 21 ->
# 29 "lexer.mll"
                ( EQ )
# 342 "lexer.ml"

  | 22 ->
# 30 "lexer.mll"
                ( COLON )
# 347 "lexer.ml"

  | 23 ->
# 31 "lexer.mll"
                ( ARROW )
# 352 "lexer.ml"

  | 24 ->
# 32 "lexer.mll"
                ( UP )
# 357 "lexer.ml"

  | 25 ->
# 33 "lexer.mll"
                ( LBRACKET )
# 362 "lexer.ml"

  | 26 ->
# 34 "lexer.mll"
                ( COMMA )
# 367 "lexer.ml"

  | 27 ->
# 35 "lexer.mll"
                ( RBRACKET )
# 372 "lexer.ml"

  | 28 ->
# 36 "lexer.mll"
                ( QUOTE )
# 377 "lexer.ml"

  | 29 ->
# 37 "lexer.mll"
                ( INTV (int_of_string (Lexing.lexeme lexbuf)) )
# 382 "lexer.ml"

  | 30 ->
# 39 "lexer.mll"
                ( STRINGV (Lexing.lexeme lexbuf) )
# 387 "lexer.ml"

  | 31 ->
# 41 "lexer.mll"
                ( STR_VAR (Lexing.lexeme lexbuf) )
# 392 "lexer.ml"

  | 32 ->
# 42 "lexer.mll"
                ( EOF )
# 397 "lexer.ml"

  | 33 ->
# 43 "lexer.mll"
                ( raise Lexical_error )
# 402 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

