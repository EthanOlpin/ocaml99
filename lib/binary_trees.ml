open Base

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
[@@deriving sexp_of, compare, equal]

(* Problem 55 *)
let tree_combine l r =
  List.concat_map l ~f:(fun l -> List.map r ~f:(fun r -> Node ('x', l, r)))
;;

let rec cbal_tree n =
  if n = 0
  then [ Empty ]
  else if n % 2 = 1
  then (
    let subtrees = cbal_tree (n / 2) in
    tree_combine subtrees subtrees)
  else (
    let subtrees_a = cbal_tree ((n / 2) - 1) in
    let subtrees_b = cbal_tree (n / 2) in
    tree_combine subtrees_a subtrees_b @ tree_combine subtrees_b subtrees_a)
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

(* Problem 59 *)
let rec hbal_tree n =
  if n = 0
  then [ Empty ]
  else if n = 1
  then [ Node ('x', Empty, Empty) ]
  else (
    let subtrees_a = hbal_tree (n - 1) in
    let subtrees_b = hbal_tree (n - 2) in
    tree_combine subtrees_a subtrees_a
    @ tree_combine subtrees_a subtrees_b
    @ tree_combine subtrees_b subtrees_a)
;;

(* Problem 61 *)
let rec count_leaves tree =
  match tree with
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, left, right) -> count_leaves left + count_leaves right
;;

(* Problem 61A *)
let rec collect_leaves tree =
  match tree with
  | Empty -> []
  | Node (x, Empty, Empty) -> [ x ]
  | Node (_, left, right) -> collect_leaves left @ collect_leaves right
;;
