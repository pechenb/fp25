(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Angstrom

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
  let* var =
    take_while1 (function
      | 'a' .. 'z' -> true
      | _ -> false)
  in
  match var with
  | "if" | "then" | "else" | "let" | "in" -> fail "Name not permitted"
  | _ -> return var
;;

let integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
;;

type dispatch =
  { apps : dispatch -> string Ast.t Angstrom.t
  ; single : dispatch -> string Ast.t Angstrom.t
  }

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let parse_miniml =
  let single pack =
    fix (fun _ ->
      choice
        [ (* аппликация в скобках *)
          (let* _ = skip_spc (char '(')
           and+ elem = pack.apps pack
           and+ _ = skip_spc (char ')') <?> "Parentheses expected" in
           return elem)
          (* функция *)
        ; (let* _ = skip_spc (string "fun")
           and+ var = skip_spc varname
           and+ _ = skip_spc (string "->")
           and+ body = pack.apps pack in
           return (Ast.Fun (var, body)))
          (* if *)
        ; (let* _ = skip_spc (string "if")
           and+ cond = pack.apps pack
           and+ _ = skip_spc (string "then")
           and+ e1 = pack.apps pack
           and+ _ = skip_spc (string "else")
           and+ e2 = pack.apps pack in
           return (Ast.If (cond, e1, e2)))
          (* let *)
        ; (let* _ = skip_spc (string "let")
           and+ var = skip_spc varname
           and+ _ = skip_spc (char '=')
           and+ e1 = pack.apps pack
           and+ _ = skip_spc (string "in")
           and+ e2 = pack.apps pack in
           return (Ast.Let (var, e1, e2)))
          (* переменная *)
        ; (let* s = skip_spc varname in
           return (Ast.Var s))
          (* целое число *)
        ; (let* i = skip_spc integer in
           return (Ast.Int (int_of_string i)))
          (* TODO: binop *)
        ])
  in
  let apps pack =
    let* app =
      many1
        (let* elem = skip_spc (pack.single pack) in
         return elem)
    in
    match app with
    | [] -> fail "bad syntax"
    | x :: xs -> return @@ List.fold_left (fun l r -> Ast.App (l, r)) x xs
  in
  { single; apps }
;;

let parse str =
  match
    Angstrom.parse_string
      (parse_miniml.apps parse_miniml)
      ~consume:Angstrom.Consume.All
      str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
