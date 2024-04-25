(* AST *)

(** Constant **)
type const = String of string

(** Expression **)
type exp = VarExp of string | ConstExp of const | TermExp of string * exp list

(** Declaration **)
type dec = Clause of exp * exp list | Query of exp list

(** Sugar **)
let atom_sugar a = TermExp (a, [])

let fact_sugar p = Clause (p, [ TermExp ("true", []) ])
