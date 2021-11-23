
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(* Esta funcion tragará hasta ;; *)
let get_exp s = 
  let in_s = read_line () in
  try 
    let pos = String.index in_s ';' 
  
  with 
    Not_found -> get_exp (s^" "in_s)


  String.index "abc;" ';';;

(*Dado un input, lo parsea, lo evalua y printa su resultado*)
let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (read_line ())) in
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
      loop ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;

