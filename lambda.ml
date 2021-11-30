(* 
  Recuerden Traducir todo caballeros.

  Dudas:
    isval -> y el tmvar?? (me acaba de rallar que flipas) 
              -> realmente lo que buscamos es cambiar las vars por valores pero porque en la 1º si que lo consideran como valor?? bu me he liao

    letrec --> que intermaente se cambie a lo otro, 1º cambio meter en el mli letrec

    - : Nat = ({1 , 2}).1 -> esto no lo reconoce la gramática y realmente no cunde que no lo haga porque un (id({1,2})).1 no lo reconocería tampoco

    en free vars tengo que tener en cuenta el contexto?


  (*| LBRACKET appTerm COMMA appTerm RBRACKET DOT INTV
      { TmProj (TmPair ($2, $4), $7)  esto esta mal, yo lo subia al termio por ejemplo, y si no pues no ponerlo tan restrictivo
      # (let x = 1 in x,1);;
        - : int * int = (1, 1)
      }*)

*)

(***********************************TYPES***********************************)
(* Base Types *)
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty (* arrow type *)
  | TyPair of ty * ty
;;


(* Términos *)
type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmPair of term * term
  | TmProj of term * int
;;

(* Command *)
type command =
    Eval of term
  | Bind of string * term
;;

(* Contexto es una lista de correspondencias entre variables libres y su tipo/valor *)
type context =
  (string * ty * term option) list
;;

exception Type_error of string;;

(*******************************CONTEXT MANAGEMENT*******************************)
(* Creates an empty context *)
let emptyctx =
  []
;;

(* Adds binding to a given context *)
let addbinding ctx x ty te =
  (x, ty, Some(te)) :: ctx
;;

let addbinding_type ctx x ty =
  (x, ty, None) :: ctx
;;

(* Gets binding to a given context *)
exception Not_Found;;

let rec getbinding_type ctx x = match ctx with
  ((a,ty,_)::t) -> if x=a then ty else getbinding_type t x
  |[] -> raise Not_Found
;;

let rec getbinding_term ctx x = match ctx with
  ((a,_,Some(term))::t) -> if x=a then term else getbinding_term t x
  |((a,_,None)::t) -> getbinding_term t x
  |[] -> raise Not_Found
;;

(******************************* Printing *******************************)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyPair (ty1, ty2) ->
      "{ " ^ string_of_ty ty1 ^ " , " ^ string_of_ty ty2 ^ "}"
;;

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix (t) ->
      "(fix " ^ string_of_term t ^ ")"
  | TmPair (t1, t2) ->
      "{" ^ string_of_term t1 ^ " , " ^ string_of_term t2 ^ "}"
  (* 
    
    Esto esta mal -------------------------------------------- molaba meter un print que copiada la expresion lo reconozca la gramática
  
  *)
  | TmProj (t, n) -> 
      "(" ^ string_of_term t ^ ")." ^ (string_of_int n)
;;


(*******************************TYPE MANAGEMENT (TYPING)*******************************)


(* Given a context and a term we find its type (Inversion Lema) *)
let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try getbinding_type ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addbinding_type ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding_type ctx x tyT1 in
      typeof ctx' t2
   
    (* T-TmFix *)
  | TmFix (t1) ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
        TyArr (tyT11,tyT12) ->
          if tyT11 = tyT12 then tyT12
          else raise (Type_error "result of body not compatible wirh domain")
        | _ -> raise (Type_error "arrow type excepted")
      )
  (* T-Pair *)
  | TmPair (t1, t2) ->
      (* ctx |- {t1,t2} : T1 X T2 *)
      let tyT1 = typeof ctx t1 
      and tyT2 = typeof ctx t2 in
      TyPair(tyT1, tyT2)

  (* T-Proj1 *)
  | TmProj (TmPair (t1, t2), n) ->
      (* ctx |- P.1 : T1 *)
      (match n with
        1 -> typeof ctx t1
        | 2 -> typeof ctx t2
        | _ -> raise (Type_error "tuple out of bounds")
      )  
  | TmProj (t, proj) -> raise (Type_error ("cannot project type " ^ string_of_term t))

;;


(*********************************** EVAL ***********************************)

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

(*Calcular vars libres *)
let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  (* TODO: revisar *)
  | TmPair (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmProj (TmPair (t1, t2), n) -> (match n with
        1 -> free_vars t1
        | 2 -> free_vars t2
        | _ -> raise (Type_error "tuple out of bounds")
      )
  | TmProj (t, proj) -> raise (Type_error ("cannot project type " ^ string_of_term t))

;;

(*
  if the name is inside the list gens a new name 
    -> Lo usamos en la substitución de las abstracciones, 
       cuando substituimos por un término el cual tiene libre justo la var que fija la abstraccion
*)
let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
(*
  substituir -> 
    - vars:                     caso base (si y==x se cambia)
    - true/false/zero:          caso base
    - apps,if,succ,pred.iszero: caso recursivo
    - abs,letin:                caso especial: 
                                  -si (x==y)            -> se deja como está
                                  -si (y no es fv de s) -> se aplica proceso rec
                                  -si (y es fv de s)    -> se genera un nuevo nombre para la abstracción y se continua el proceso recursivo

*)
let rec subst ctx x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst ctx x s t1, subst ctx x s t2, subst ctx x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst ctx x s t)
  | TmPred t ->
      TmPred (subst ctx x s t)
  | TmIsZero t ->
      TmIsZero (subst ctx x s t)
  | TmVar y ->
      (* REFLEXION INTERESANTE:
        revisar el contexto iria en el else para dar un comportamiento de 
        cercanía (de hecho los bindings se van apilando y el primer tipo en encontrar será el mas cercano)
      *)
      (* MAL CAMBIAR *)
      if y = x then s else tm
       
      (* Esto va en el eval no en el subs  *)
      (*(try getbinding_term ctx x with _ -> tm)
         acabar de hacer *)
        

      
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst ctx x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst ctx x s (subst ctx y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst ctx x s t1, subst ctx x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst ctx x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst ctx x s t1, subst ctx x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst ctx x s t1, subst ctx x s (subst ctx y (TmVar z) t2))
  | TmFix t -> 
      TmFix (subst ctx x s t)
  (* TODO: revisar *)
  | TmPair (t1, t2) ->
      TmPair ((subst ctx x s t1), (subst ctx x s t2))
  | TmProj (TmPair (t1, t2), n) -> (match n with
        1 -> subst ctx x s t1
        | 2 -> subst ctx x s t2
        | _ -> raise (Type_error "tuple out of bounds")
      )
  | TmProj (t, proj) -> raise (Type_error ("cannot project type " ^ string_of_term t))
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

(*tmvar no es un valor??? valores*)
let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;

(* Evaluamos *)
let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst ctx x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst ctx x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2) 

    (* E-FixBeta *)
  | TmFix (TmAbs (x,_,t12)) ->
      subst ctx x tm t12

    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 ctx t1 in
      TmFix t1'

  | TmPair (t1, t2) -> (try
      (* E-Pair1 *)
      let t1' = eval1 ctx t1 in
      TmPair (t1', t2)
    with NoRuleApplies -> 
      (* E-Pair2 *)
      let t2' = eval1 ctx t2 in 
      TmPair (t1, t2'))

  | TmProj (TmPair (t1, t2), n) -> (match n with
        (* E-Proj1 *)
        (* E-PairBeta1 *)
        1 -> t1
        (* E-Proj2 *)
        (* E-PairBeta2 *)
        | 2 -> t2
        | _ -> raise (Type_error "tuple out of bounds")
      )
  | TmProj (t, proj) -> raise (Type_error ("cannot project type " ^ string_of_term t))
  
  | TmVar x ->  (try getbinding_term ctx x 
                with _ -> raise NoRuleApplies) (* REALMENTE INNECESARIO si no esta en ctx cascarian los tipos *)
                (* 
                
                 *)

  | _ ->
      raise NoRuleApplies
;;

let rec subs_ctx ctx tm vl = match tm with
    TmTrue -> TmTrue
  | TmFalse -> TmFalse
  | TmIf (t1, t2, t3) -> TmIf (subs_ctx ctx t1 vl, subs_ctx ctx t2 vl, subs_ctx ctx t3 vl) 
  | TmZero -> TmZero
  | TmSucc t -> TmSucc (subs_ctx ctx t vl)
  | TmPred t -> TmPred (subs_ctx ctx t vl)
  | TmIsZero t -> TmIsZero (subs_ctx ctx t vl)
  | TmVar x -> 
    if List.mem x vl then
      tm
    else
      (try getbinding_term ctx x with _ -> tm) (* Realmente si es libre y no esta en el contexto ya cascan los tipos*)
  | TmAbs (y, tyY, t) -> TmAbs (y, tyY, subs_ctx ctx t (y::vl))
  | TmApp (t1, t2) -> TmApp (subs_ctx ctx t1 vl, subs_ctx ctx t2 vl)
  | TmLetIn (y, t1, t2) -> TmLetIn (y, subs_ctx ctx t1 vl, subs_ctx ctx t2 (y::vl))
  | TmFix t ->  TmFix (subs_ctx ctx t vl)
  | TmPair (t1, t2) -> TmPair ((subs_ctx ctx t1 vl), (subs_ctx ctx t2 vl))
  | TmProj (t, proj) -> TmProj (subs_ctx ctx t vl, proj)
;;

(* Evaluate until no more terms can be evaluated *)
let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
      print_endline ("\t" ^ string_of_term (tm') ^ " : (falta meter el tipo)");
      eval ctx tm'
  with
    NoRuleApplies -> subs_ctx ctx tm []
;;
