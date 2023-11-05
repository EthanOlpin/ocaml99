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

(* Problem 48 *)
let evaluate variables expr =
  let rec aux expr =
    match expr with
    | Var x -> List.Assoc.find_exn variables ~equal:String.equal x
    | And (l, r) -> aux l && aux r
    | Or (l, r) -> aux l || aux r
    | Not expr -> not (aux expr)
  in
  aux expr
;;

let rec bool_combinations len =
  if len = 0
  then [ [] ]
  else (
    let prepend_all x = List.map ~f:(fun l -> x :: l) in
    let combs = bool_combinations (len - 1) in
    prepend_all true combs @ prepend_all false combs)
;;

let table variables expr =
  let len = List.length variables in
  let combs = bool_combinations len in
  List.map combs ~f:(fun comb ->
    let variables = List.zip_exn variables comb in
    variables, evaluate variables expr)
;;

(* Problem 49 *)
let rec gray n =
  if n = 0
  then [ "" ]
  else (
    let prepend_all x = List.map ~f:(fun s -> x ^ s) in
    let sub_list = gray (n - 1) in
    prepend_all "0" sub_list @ prepend_all "1" (List.rev sub_list))
;;
