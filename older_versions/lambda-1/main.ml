
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(*  
  Dudas
    -que es el __s__ token -> en funcs del lexer/parser me pierdo algo

  Cosas que hacer:
    -Investigar funcionamiento lexer/parser

  Dado un input, lo parsea, lo evalua y printa su resultado
*)
let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop () =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (read_line ())) in
      print_endline (string_of_term (eval tm));
      loop ()
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ()
     | Parse_error ->
         print_endline "syntax error";
         loop ()
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop ()
  ;;

top_level_loop ()
;;

