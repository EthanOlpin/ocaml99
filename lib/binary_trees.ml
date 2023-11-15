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

(* Problem 60 *)
(* Referred to official solution *)
let max_nodes h = if h <= 0 then 0 else (2 ** h) - 1

let min_nodes h =
  if h <= 0
  then 0
  else (
    let rec aux h prev_1 prev_2 =
      if h <= 1 then prev_1 else aux (h - 1) (prev_1 + prev_2 + 1) prev_1
    in
    aux h 1 0)
;;

let min_height n = if n = 0 then 0 else Int.floor_log2 n + 1

let max_height n =
  let rec aux h prev_min_1 prev_min_2 =
    if prev_min_1 > n
    then h
    else aux (h + 1) (prev_min_1 + prev_min_2 + 1) prev_min_1
  in
  aux 0 1 0
;;

let mirror = function
  | Node (x, l, r) -> Some (Node (x, r, l))
  | Empty -> None
;;

let hbal_tree_nodes n =
  let rec hbal_trees_height_nodes h n =
    assert (min_nodes h <= n && n <= max_nodes h);
    if h = 0
    then [ Empty ]
    else (
      let build_and_join_subtrees l_height r_height =
        let min_n = max (min_nodes l_height) (n - 1 - max_nodes r_height) in
        let max_n = min (max_nodes l_height) (n - 1 - min_nodes r_height) in
        List.range ~stop:`inclusive min_n max_n
        |> List.concat_map ~f:(fun curr_n ->
          let ls = hbal_trees_height_nodes l_height curr_n in
          let rs = hbal_trees_height_nodes r_height (n - 1 - curr_n) in
          List.cartesian_product ls rs
          |> List.map ~f:(fun (l, r) -> Node ('x', l, r)))
      in
      let nearly_unbalanced = build_and_join_subtrees (h - 1) (h - 2) in
      let mirror_nearly_unbalanced =
        List.filter_map nearly_unbalanced ~f:mirror
      in
      let perfectly_balanced = build_and_join_subtrees (h - 1) (h - 1) in
      nearly_unbalanced @ mirror_nearly_unbalanced @ perfectly_balanced)
  in
  List.range ~stop:`inclusive (min_height n) (max_height n)
  |> List.concat_map ~f:(fun h -> hbal_trees_height_nodes h n)
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

(* Problem 62 *)
let internals tree =
  let rec aux acc = function
    | Empty | Node (_, Empty, Empty) -> acc
    | Node (x, left, right) -> aux (x :: aux acc right) left
  in
  aux [] tree
;;

(* Problem 62B *)
let at_level tree n =
  let rec aux tree level_i acc =
    match tree with
    | Empty -> []
    | Node (x, left, right) ->
      if level_i = n
      then x :: acc
      else (
        let level_i = level_i + 1 in
        aux left level_i (aux right level_i acc))
  in
  aux tree 1 []
;;

(* Problem 63 *)
(* Referred to official solution *)
let size tree =
  let rec aux tree acc =
    match tree with
    | Node (_, l, r) -> aux l (acc + aux r 1)
    | Empty -> acc
  in
  aux tree 0
;;

let is_complete_binary_tree tree =
  let tree_size = size tree in
  let rec aux node curr_address =
    match node with
    | Empty -> true
    | Node (_, l, r) ->
      curr_address <= tree_size
      && aux l (2 * curr_address)
      && aux r ((2 * curr_address) + 1)
  in
  aux tree 1
;;

let rec distribute vals nodes =
  match vals, nodes with
  | vals, [] -> List.map vals ~f:(fun x -> Node (x, Empty, Empty))
  | x :: vals, [ l ] -> Node (x, l, Empty) :: distribute vals []
  | x :: vals, l :: r :: nodes -> Node (x, l, r) :: distribute vals nodes
  | _ -> failwith "Cannot distribute empty value set across nodes"
;;

let complete_binary_tree vals =
  match vals with
  | [] -> Empty
  | vals ->
    let rec integrate_level lvl vals =
      match vals with
      | [] -> []
      | vals ->
        let lvl_vals, rest = List.split_n vals ((lvl ** 2) + 1) in
        distribute lvl_vals (integrate_level (lvl + 1) rest)
    in
    List.hd_exn (integrate_level 0 vals)
;;
