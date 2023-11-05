open Base

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

(* Problem 46 & Problem 47 *)
let evaluate2 a a_val b b_val expr =
  let rec aux expr =
    match expr with
    | Var x ->
      if String.equal x a
      then a_val
      else if String.equal x b
      then b_val
      else Printf.sprintf "Unrecognized variable: %s" x |> failwith
    | And (l, r) -> aux l && aux r
    | Or (l, r) -> aux l || aux r
    | Not expr -> not (aux expr)
  in
  aux expr
;;

let table2 a b expr =
  let eval a_val b_val = a_val, b_val, evaluate2 a a_val b b_val expr in
  [ eval true true; eval true false; eval false true; eval false false ]
;;
