open Base
open Ocaml99.Binary_trees

(* Problem 55 *)
let%test_unit "cbal_tree" =
  [%test_eq: char binary_tree list]
    (cbal_tree 4)
    [ Node
        ( 'x'
        , Node ('x', Empty, Empty)
        , Node ('x', Empty, Node ('x', Empty, Empty)) )
    ; Node
        ( 'x'
        , Node ('x', Empty, Empty)
        , Node ('x', Node ('x', Empty, Empty), Empty) )
    ; Node
        ( 'x'
        , Node ('x', Empty, Node ('x', Empty, Empty))
        , Node ('x', Empty, Empty) )
    ; Node
        ( 'x'
        , Node ('x', Node ('x', Empty, Empty), Empty)
        , Node ('x', Empty, Empty) )
    ];
  [%test_eq: int] (List.length (cbal_tree 40)) 524288
;;

(* Problem 56 *)
let%test_unit "is_symmetric" =
  let leaf = Node (0, Empty, Empty) in
  [%test_eq: bool] (is_symmetric (Node (0, leaf, Empty))) false;
  [%test_eq: bool] (is_symmetric (Node (0, leaf, leaf))) true
;;

(* Problem 57 *)
let%test_unit "construct" =
  [%test_eq: int binary_tree]
    (construct [ 3; 2; 5; 7; 1 ])
    (Node
       ( 3
       , Node (2, Node (1, Empty, Empty), Empty)
       , Node (5, Empty, Node (7, Empty, Empty)) ));
  [%test_eq: bool] (is_symmetric (construct [ 5; 3; 18; 1; 4; 12; 21 ])) true;
  [%test_eq: bool] (is_symmetric (construct [ 3; 2; 5; 7; 4 ])) false
;;

(* Problem 58 *)
let%test_unit "sym_cbal_trees" =
  [%test_eq: char binary_tree list]
    (sym_cbal_trees 5)
    [ Node
        ( 'x'
        , Node ('x', Empty, Node ('x', Empty, Empty))
        , Node ('x', Node ('x', Empty, Empty), Empty) )
    ; Node
        ( 'x'
        , Node ('x', Node ('x', Empty, Empty), Empty)
        , Node ('x', Empty, Node ('x', Empty, Empty)) )
    ]
;;

(* Problem 59 *)
let%test_unit "hbal_tree" =
  let t = hbal_tree 3 in
  let x = 'x' in
  [%test_eq: bool]
    (List.mem
       t
       (Node
          ( x
          , Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty))
          , Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)) ))
       ~equal:(equal_binary_tree Char.equal))
    true;
  [%test_eq: bool]
    (List.mem
       t
       (Node
          ( x
          , Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty))
          , Node (x, Node (x, Empty, Empty), Empty) ))
       ~equal:(equal_binary_tree Char.equal))
    true;
  [%test_eq: int] (List.length t) 15
;;

(* Problem 61 *)
let%test_unit "count_leaves" =
  let leaf = Node (0, Empty, Empty) in
  [%test_eq: int] (count_leaves Empty) 0;
  [%test_eq: int] (count_leaves (Node (0, leaf, Node (0, leaf, leaf)))) 3
;;
