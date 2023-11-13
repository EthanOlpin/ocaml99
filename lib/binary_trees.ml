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
