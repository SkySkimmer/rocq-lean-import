
module U : sig
  type t = Prop | Succ of t | Max of t * t | IMax of t * t | UNamed of LeanName.t
end

type binder_kind =
  | NotImplicit
  | Maximal
  | NonMaximal
  | Typeclass  (** WRT Coq, Typeclass is like Maximal. *)

type expr =
  | Bound of int
  | Sort of U.t
  | Const of LeanName.t * U.t list
  | App of expr * expr
  | Let of { name : LeanName.t; ty : expr; v : expr; rest : expr }
      (** Let: undocumented in export_format.md *)
  | Lam of binder_kind * LeanName.t * expr * expr
  | Pi of binder_kind * LeanName.t * expr * expr
  | Proj of LeanName.t * int * expr  (** Proj: name of ind, field, term *)
  | Nat of Z.t
  | String of string

type def = { ty : expr; body : expr; univs : LeanName.t list; height : int }
type ax = { ty : expr; univs : LeanName.t list }

type ind = {
  params : (binder_kind * LeanName.t * expr) list;
  ty : expr;
  ctors : (LeanName.t * expr) list;
  univs : LeanName.t list;
}

type entry = Def of def | Ax of ax | Ind of ind | Quot
