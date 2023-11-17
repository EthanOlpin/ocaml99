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

(* Problem 64 *)
let layout_binary_tree_1 tree =
  let rec position_nodes node offset depth =
    match node with
    | Empty -> offset, Empty
    | Node (v, l, r) ->
      let depth' = depth + 1 in
      let l_offset, l' = position_nodes l offset depth' in
      let r_offset, r' = position_nodes r (l_offset + 1) depth' in
      r_offset, Node ((v, l_offset, depth), l', r')
  in
  snd (position_nodes tree 1 1)
;;

(* Problem 65 *)
let rec tree_height = function
  | Empty -> 0
  | Node (_, l, r) -> 1 + max (tree_height l) (tree_height r)
;;

let layout_binary_tree_2 tree =
  let left_width tree =
    let rec left_depth depth = function
      | Empty -> depth
      | Node (_, l, _) -> left_depth (depth + 1) l
    in
    (2 ** (left_depth 1 tree - 1)) - 1
  in
  let height = tree_height tree in
  let rec position_nodes offset depth = function
    | Empty -> Empty
    | Node (v, l, r) ->
      let dist_to_child =
        if height = depth then 0 else 2 ** (height - depth - 1)
      in
      let l' = position_nodes (offset - dist_to_child) (depth + 1) l in
      let r' = position_nodes (offset + dist_to_child) (depth + 1) r in
      Node ((v, offset, depth), l', r')
  in
  let left_width = left_width tree in
  position_nodes left_width 1 tree
;;

(* Problem 66 *)
let rec merge_bounds a b =
  let merge (al, ar) (bl, br) = min al bl, max ar br in
  match a, b with
  | a_hd :: a_rest, b_hd :: b_rest ->
    merge a_hd b_hd :: merge_bounds a_rest b_rest
  | a, [] -> a
  | [], b -> b
;;

let rec max_overlap a b acc =
  match a, b with
  | (_, ar) :: a_rest, (bl, _) :: b_rest ->
    max_overlap a_rest b_rest (max acc (1 + ar - bl))
  | _, [] | [], _ -> acc
;;

let shift_bounds shift = List.map ~f:(fun (al, ar) -> al + shift, ar + shift)

let layout_binary_tree_3 tree =
  let rec get_spread offset = function
    | Empty -> [], Empty
    | Node (v, l, r) ->
      let l_bounds, l' = get_spread (offset - 1) l in
      let r_bounds, r' = get_spread (offset + 1) r in
      let spread = max_overlap l_bounds r_bounds 0 in
      let l_bounds = shift_bounds (-spread) l_bounds in
      let r_bounds = shift_bounds spread r_bounds in
      let bounds = (offset, offset) :: merge_bounds l_bounds r_bounds in
      bounds, Node ((1 + spread, v), l', r')
  in
  let rec left_width acc = function
    | Empty -> acc
    | Node ((spread, _), l, _) -> left_width (acc + spread) l
  in
  let rec layout_tree offset depth = function
    | Empty -> Empty
    | Node ((spread, v), l, r) ->
      let depth' = depth + 1 in
      let l' = layout_tree (offset - spread) depth' l in
      let r' = layout_tree (offset + spread) depth' r in
      Node ((v, offset, depth), l', r')
  in
  let _, spread_tree = get_spread 0 tree in
  let left_width = left_width 0 spread_tree in
  layout_tree left_width 1 spread_tree
;;

(* Problem 67 *)
let rec string_of_tree = function
  | Empty -> ""
  | Node (v, Empty, Empty) -> String.of_char v
  | Node (v, l, r) ->
    Printf.sprintf "%c(%s,%s)" v (string_of_tree l) (string_of_tree r)
;;

let rec tree_of_string s =
  if String.equal s ""
  then Empty
  else (
    let v = String.get s 0 in
    let len = String.length s in
    if len = 1
    then Node (v, Empty, Empty)
    else (
      let content = String.sub s ~pos:2 ~len:(len - 3) in
      let i =
        String.fold_until
          content
          ~init:(0, 0)
          ~f:(fun (parens, i) ->
            let i' = i + 1 in
            function
            | ',' when parens = 0 -> Stop i
            | '(' -> Continue (parens + 1, i')
            | ')' -> Continue (parens - 1, i')
            | _ -> Continue (parens, i'))
          ~finish:(fun _ -> failwith ("failed to parse node: " ^ s))
      in
      let l_s, r_s = String.(subo content ~len:i, subo content ~pos:(i + 1)) in
      Node (v, tree_of_string l_s, tree_of_string r_s)))
;;

(* Problem 68 *)
let rec preorder = function
  | Empty -> []
  | Node (v, l, r) -> (v :: preorder l) @ preorder r
;;

let inorder tree =
  let rec aux acc = function
    | Empty -> acc
    | Node (v, l, r) -> aux (v :: aux acc l) r
  in
  List.rev (aux [] tree)
;;
