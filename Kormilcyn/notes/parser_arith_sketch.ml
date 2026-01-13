(* Sketch of arithmetic parsing with precedence, matching current dispatch/fix style. *)
open Angstrom

(* Minimal helpers mirroring lib/parser.ml *)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space

let skip_spc p =
  let* _ = spaces
  and+ payload = p
  and+ _ = spaces in
  return payload
;;

let varname =
  take_while1 (function
    | 'a' .. 'z' -> true
    | _ -> false)
;;

let integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
;;

module Ast = struct
  type binop =
    | Add
    | Mul
    | Sub
    | Div

  type 'a t =
    | Var of 'a
    | Fun of 'a * 'a t
    | App of 'a t * 'a t
    | Int of int
    | Bin of binop * 'a t * 'a t
    | Let of 'a * 'a t * 'a t
    | If of 'a t * 'a t * 'a t
end

type dispatch =
  { binop : dispatch -> Ast.binop -> string Ast.t Angstrom.t
  ; apps : dispatch -> string Ast.t Angstrom.t
  ; single : dispatch -> string Ast.t Angstrom.t
  }

let chainl1 p op =
  let rec loop acc =
    (let* f = op
     and+ y = p in
     loop (f acc y))
    <|> return acc
  in
  let* x = p in
  loop x
;;

let chainl1 p op =
  let rec loop acc =
    (let* f = op
     and+ y = p in
     loop (f acc y))
    <|> return acc
  in
  let* x = p in
  loop x
;;

(* single через fix, но только атомы *)
let single pack =
  fix (fun atom ->
    choice
      [ (let* _ = skip_spc (char '(')
         and+ e = pack.apps pack
         and+ _ = skip_spc (char ')') in
         return e)
      ; (let* _ = skip_spc (string "fun")
         and+ v = skip_spc varname
         and+ _ = skip_spc (string "->")
         and+ b = pack.apps pack in
         return (Ast.Fun (v, b)))
      ; (let* _ = skip_spc (string "if")
         and+ c = pack.apps pack
         and+ _ = skip_spc (string "then")
         and+ e1 = pack.apps pack
         and+ _ = skip_spc (string "else")
         and+ e2 = pack.apps pack in
         return (Ast.If (c, e1, e2)))
      ; (let* _ = skip_spc (string "let")
         and+ v = skip_spc varname
         and+ _ = skip_spc (char '=')
         and+ e1 = pack.apps pack
         and+ _ = skip_spc (string "in")
         and+ e2 = pack.apps pack in
         return (Ast.Let (v, e1, e2)))
      ; (skip_spc varname >>| fun v -> Ast.Var v)
      ; (skip_spc integer >>| fun i -> Ast.Int (int_of_string i))
      ])
;;

(* аппликация поверх атомов *)
let apps pack =
  let* xs = many1 (skip_spc (pack.single pack)) in
  match xs with
  | x :: tl -> return (List.fold_left (fun l r -> Ast.App (l, r)) x tl)
  | [] -> fail "bad syntax"
;;

(* уровни приоритета *)
let mul_div pack =
  let op =
    choice
      [ skip_spc (char '*') *> return (fun l r -> Ast.Bin (Ast.Mul, l, r))
      ; skip_spc (char '/') *> return (fun l r -> Ast.Bin (Ast.Div, l, r))
      ]
  in
  chainl1 (apps pack) op
;;

let add_sub pack =
  let op =
    choice
      [ skip_spc (char '+') *> return (fun l r -> Ast.Bin (Ast.Add, l, r))
      ; skip_spc (char '-') *> return (fun l r -> Ast.Bin (Ast.Sub, l, r))
      ]
  in
  chainl1 (mul_div pack) op
;;

(* Итоговый диспетчер; binop не нужен, можно заглушку. *)
let parse_miniml =
  { binop = (fun _ _ -> fail "binop unused in sketch"); single; apps = add_sub }
;;
