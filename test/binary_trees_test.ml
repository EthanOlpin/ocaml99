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

let example_tree =
  Node
    ( 'a'
    , Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty))
    , Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)) )
;;

(* Problem 60*)
let%test_unit "min_nodes" =
  [%test_eq: int] (min_nodes 0) 0;
  [%test_eq: int] (min_nodes 1) 1;
  [%test_eq: int] (min_nodes 2) 2;
  [%test_eq: int] (min_nodes 3) 4;
  [%test_eq: int] (min_nodes 4) 7
;;

let%test_unit "min_height" =
  [%test_eq: int] (min_height 0) 0;
  [%test_eq: int] (min_height 1) 1;
  [%test_eq: int] (min_height 2) 2;
  [%test_eq: int] (min_height 3) 2;
  [%test_eq: int] (min_height 4) 3
;;

let%test_unit "max_height" =
  [%test_eq: int] (max_height 0) 0;
  [%test_eq: int] (max_height 1) 1;
  [%test_eq: int] (max_height 2) 2;
  [%test_eq: int] (max_height 4) 3;
  [%test_eq: int] (max_height 7) 4
;;

let%test_unit "hbal_tree_nodes" =
  [%test_eq: int] (hbal_tree_nodes 15 |> List.length) 1553;
  [%test_eq: char binary_tree list list]
    (List.map [ 0; 1; 2; 3 ] ~f:hbal_tree_nodes)
    [ [ Empty ]
    ; [ Node ('x', Empty, Empty) ]
    ; [ Node ('x', Node ('x', Empty, Empty), Empty)
      ; Node ('x', Empty, Node ('x', Empty, Empty))
      ]
    ; [ Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)) ]
    ]
;;

(* Problem 61 *)
let%test_unit "count_leaves" =
  [%test_eq: int] (count_leaves Empty) 0;
  [%test_eq: int] (count_leaves example_tree) 3
;;

(* Problem 61A *)
let%test_unit "collect_leaves" =
  [%test_eq: char list] (collect_leaves Empty) [];
  [%test_eq: char list] (collect_leaves example_tree) [ 'd'; 'e'; 'g' ]
;;

(* Problem 62*)
let%test_unit "internals" =
  [%test_eq: char list] (internals (Node ('a', Empty, Empty))) [];
  [%test_eq: char list] (internals example_tree) [ 'b'; 'a'; 'c'; 'f' ]
;;

(* Problem 62B*)
let%test_unit "at_level" =
  [%test_eq: char list] (at_level example_tree 2) [ 'b'; 'c' ];
  [%test_eq: char list] (at_level example_tree 5) []
;;
