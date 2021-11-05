(* 
  Tipos -> simplemente vars
*)
type term =
    TmVar of string
  | TmAbs of string * term
  | TmApp of term * term
;;

(* 
  Simple Printer
*)
let rec string_of_term tm = match tm with
    TmVar s -> s
  | TmAbs (s, t) -> "(lambda " ^ s ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
;;

(***********************************EVAL***********************************)

(* l1 - l2 (listas) *)
let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

(* l1 u l2 (listas) *)
let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

(*
  Calcular vars libres ->   
    - caso base Var
    - casos no triviales: aplicaciones/abstracciones
*)
let rec free_vars tm = match tm with
    TmVar s -> [s]
  | TmAbs (s, t) -> ldif (free_vars t) [s]
  | TmApp (t1, t2) -> lunion (free_vars t1) (free_vars t2)
;;

(*
  te añade comillas al nombre hasta que no esté en la lista -> nombre bien nuevo y fresco :)
*)
let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
 
(*
  substituir -> 
    - vars: caso base (si y==x se cambia)
    - apps: caso recursivo
    - abs:  caso especial: 
                  -si (x==y)            -> se deja como está
                  -si (y no es fv de s) -> se aplica proceso rec
                  -si (y es fv de s)    -> se genera un nuevo nombre para la abstracción y se continua el proceso recursivo
*)
let rec subst x s tm = match tm with
    TmVar y ->
      if y = x then s else tm
  | TmAbs (y, t) -> 
      if y = x then tm                       
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
;;

(*abs y vars son valores*)
let isval tm = match tm with
    TmVar _ -> true
  | TmAbs _ -> true
  | _ -> false
;;

exception NoRuleApplies
;;

(* como evaluar -> resolver abstracciones, 1º fun, 2º arg *)
let rec eval1 tm = match tm with
    (* E-AppAbs *)
    TmApp (TmAbs (x, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 t1 in
      TmApp (t1', t2)

  | _ ->
      raise NoRuleApplies
;;

(* evalua hasta que no pueda evaluar nada más*)
let rec eval tm =
  try
    let tm' = eval1 tm in
    eval tm'
  with
    NoRuleApplies -> tm
  ;;

  
(***********************************EVAL***********************************)


