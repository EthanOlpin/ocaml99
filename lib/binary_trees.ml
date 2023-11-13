open Base

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
[@@deriving sexp_of, compare]

(* Problem 55 *)
let rec cbal_tree n =
  if n = 0
  then [ Empty ]
  else (
    let combine l r =
      List.concat_map l ~f:(fun l -> List.map r ~f:(fun r -> Node ('x', l, r)))
    in
    if n % 2 = 1
    then (
      let subtrees = cbal_tree (n / 2) in
      combine subtrees subtrees)
    else (
      let subtrees_a = cbal_tree ((n / 2) - 1) in
      let subtrees_b = cbal_tree (n / 2) in
      combine subtrees_a subtrees_b @ combine subtrees_b subtrees_a))
;;

(* Problem 56 *)
let is_symmetric tree =
  let rec is_mirror tree_a tree_b =
    match tree_a, tree_b with
    | Empty, Empty -> true
    | Node (_, a_left, a_right), Node (_, b_left, b_right) ->
      is_mirror a_left b_right && is_mirror a_right b_left
    | _ -> false
  in
  match tree with
  | Node (_, left, right) -> is_mirror left right
  | Empty -> true
;;

(* Problem 57 *)
let construct vals =
  let rec insert parent new_val =
    match parent with
    | Empty -> Node (new_val, Empty, Empty)
    | Node (x, l, r) ->
      if new_val < x
      then Node (x, insert l new_val, r)
      else Node (x, l, insert r new_val)
  in
  let rec insert_all root vals =
    match vals with
    | [] -> root
    | x :: rest -> insert_all (insert root x) rest
  in
  insert_all Empty vals
;;

(* Problem 58 *)
let sym_cbal_trees n = cbal_tree n |> List.filter ~f:is_symmetric
