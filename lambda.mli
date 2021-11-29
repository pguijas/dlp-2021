
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyPair of ty * ty
;;


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

type command =
    Eval of term
  | Bind of string * term
;;

type context =
  (string * ty * term option) list
;;

val emptyctx : context;;
val addbinding_type : context -> string -> ty -> context;;
val getbinding_type : context -> string -> ty;;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : term -> term;;

