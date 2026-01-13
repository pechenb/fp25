type name = string

type binop =
  | Add
  | Mul
  | Sub
  | Div

type 'name t =
  | Var of 'name
  | Int of int
  | Bool of bool
  | Bin of binop * 'name t * 'name t
  | Let of 'name * 'name t * 'name t
  | Fun of 'name * 'name t * 'name t
  | If of 'name t * 'name t * 'name t
