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

type 'a node =
  | Leaf of
      { weight : int
      ; data : 'a
      }
  | Internal of
      { weight : int
      ; left : 'a node
      ; right : 'a node
      }

(* Problem 50 *)
(* Referenced: https://github.com/backtracking/bheap *)
module MinHeap = struct
  type 'a t =
    { mutable len : int
    ; mutable items : 'a array
    ; fill_val : 'a
    ; compare : 'a -> 'a -> int
    }

  let create ~len fill_val compare =
    { len = 0; items = Array.create ~len fill_val; fill_val; compare }
  ;;

  let length t = t.len

  let double t =
    let len = t.len in
    let len' = 2 * len in
    let items = t.items in
    let items' = Array.create ~len:len' t.fill_val in
    Array.blit ~src:items ~src_pos:0 ~dst:items' ~dst_pos:0 ~len;
    t.items <- items'
  ;;

  let push t x =
    let len = t.len in
    if len = Array.length t.items then double t;
    let items = t.items in
    let rec moveup i =
      let fi = (i - 1) / 2 in
      if i > 0 && t.compare items.(fi) x > 0
      then (
        items.(i) <- items.(fi);
        moveup fi)
      else items.(i) <- x
    in
    moveup len;
    t.len <- len + 1
  ;;

  let of_list l fill_val compare =
    let len = List.length l in
    let t = create ~len fill_val compare in
    List.iter l ~f:(push t);
    t
  ;;

  let pop t =
    if t.len <= 0 then failwith "Attempted to pop from empty heap";
    let len = t.len - 1 in
    t.len <- len;
    let items = t.items in
    let result = t.items.(0) in
    let x = items.(len) in
    items.(len) <- t.fill_val;
    let rec movedown t items len i x =
      let j = (2 * i) + 1 in
      if j < len
      then (
        let j =
          let j' = j + 1 in
          if j' < len && t.compare items.(j') items.(j) < 0 then j' else j
        in
        if t.compare items.(j) x < 0
        then (
          items.(i) <- items.(j);
          movedown t items len j x)
        else items.(i) <- x)
      else items.(i) <- x
    in
    movedown t items len 0 x;
    result
  ;;
end

let node_weight x =
  match x with
  | Leaf x -> x.weight
  | Internal x -> x.weight
;;

let node_compare a b = node_weight a - node_weight b

let huffman fs =
  let nodes =
    List.map fs ~f:(fun (symbol, freq) -> Leaf { weight = freq; data = symbol })
  in
  let pq =
    MinHeap.of_list nodes (Leaf { weight = -1; data = "" }) node_compare
  in
  while MinHeap.length pq > 1 do
    let left = MinHeap.pop pq in
    let right = MinHeap.pop pq in
    let new_node =
      Internal { weight = node_weight left + node_weight right; left; right }
    in
    MinHeap.push pq new_node
  done;
  let root = MinHeap.pop pq in
  let rec aux curr code acc =
    match curr with
    | Internal { left; right; _ } ->
      aux left (code ^ "0") acc @ aux right (code ^ "1") acc
    | Leaf { data = symbol; _ } -> (symbol, code) :: acc
  in
  aux root "" []
;;
