
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(* 
  que utilidad tiene que el contexto este fijado en el bucle si siempre es el mismo
*)

(* Esta funcion tragará hasta ;; *)

exception Not_Ending;;

let rec check_exp l p = match l with
  | ""::[]    -> raise (Not_Ending) (* when the expresion ends with ; (not ;;)*)
  | []        -> raise (Not_Ending)
  | ""::t     -> List.rev p
  | h::t      -> check_exp t (h::p)
;;

let rec get_exp s = 
  try 
    check_exp (String.split_on_char ';' s) []
  with 
    Not_Ending -> get_exp (s^" "^(read_line ()))
;;

(* Tokenizing and evaluating a list of expresions... (redactar molon) *)
let rec exec exp ctx = match exp with
  | [] -> ()
  | h::t -> 
      let name,tm = s token (from_string (h)) in
      let tyTm = typeof ctx tm in
      match name with
        | "" -> print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term (eval tm));
        | _ -> print_endline ("val " ^ name ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term (eval tm) );
      ;
      exec t ctx
;;

(*Dado un input, lo parsea, lo evalua y printa su resultado*)
let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      (* De momento el contexto no lo tocamos, en caunto se pueda actualizar ojocuidao -> vamos a tener que ir actualizandolo x instuccion *)
      exec (get_exp (read_line ())) ctx;
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

