
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;


let usage_msg = "top [--debug]"
let debug = ref false
let input_files = ref []
let output_file = ref ""
let anon_fun filename =
       input_files := filename::!input_files
let speclist = [("--debug", Arg.Set debug, "Output debug information"); ("-d", Arg.Set debug, "Output debug information")]

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
  | [] -> ctx
  | h::t -> 
      match s token (from_string (h)) with
        | Eval tm  -> 
            print_endline ("- : " ^ (string_of_ty (typeof ctx tm)) ^ " = " ^ string_of_term (eval ctx tm (!debug)));
            exec t ctx
        | Bind (name,tm) -> 
            let tm_eval = eval ctx tm (!debug) in
              print_endline ("val " ^ name ^ " : " ^ (string_of_ty (typeof ctx tm)) ^ " = " ^ string_of_term (tm_eval) );
              exec t (addbinding ctx name (typeof ctx tm) (tm_eval)) 
;;

(*Dado un input, lo parsea, lo evalua y printa su resultado*)
let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      (* De momento el contexto no lo tocamos, en caunto se pueda actualizar ojocuidao -> vamos a tener que ir actualizandolo x instuccion *)
      (* Ya puedo explicarlo bien que si no no se entiende un pijo *)
      loop (exec (get_exp (read_line ())) ctx);
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

Arg.parse speclist anon_fun usage_msg;
top_level_loop ()
;;

