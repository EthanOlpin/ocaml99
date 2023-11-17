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

(* Problem 63*)
let%test_unit "size" = [%test_eq: int] (size example_tree) 7

let%test "is_complete_binary_tree" =
  is_complete_binary_tree
    (Node
       ( 1
       , Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty))
       , Node (3, Node (6, Empty, Empty), Empty) ))
;;

let%test_unit "complete_binary_tree" =
  [%test_eq: int binary_tree]
    (complete_binary_tree [ 1; 2; 3; 4; 5; 6 ])
    (Node
       ( 1
       , Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty))
       , Node (3, Node (6, Empty, Empty), Empty) ))
;;

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node
    ( 'n'
    , Node
        ( 'k'
        , Node ('c', leaf 'a', Node ('h', Node ('g', leaf 'e', Empty), Empty))
        , leaf 'm' )
    , Node ('u', Node ('p', Empty, Node ('s', leaf 'q', Empty)), Empty) )
;;

let%test_unit "layout_binary_tree_1" =
  [%test_eq: (char * int * int) binary_tree]
    (layout_binary_tree_1 example_layout_tree)
    (Node
       ( ('n', 8, 1)
       , Node
           ( ('k', 6, 2)
           , Node
               ( ('c', 2, 3)
               , Node (('a', 1, 4), Empty, Empty)
               , Node
                   ( ('h', 5, 4)
                   , Node (('g', 4, 5), Node (('e', 3, 6), Empty, Empty), Empty)
                   , Empty ) )
           , Node (('m', 7, 3), Empty, Empty) )
       , Node
           ( ('u', 12, 2)
           , Node
               ( ('p', 9, 3)
               , Empty
               , Node (('s', 11, 4), Node (('q', 10, 5), Empty, Empty), Empty)
               )
           , Empty ) ))
;;

let%test_unit "layout_binary_tree_2" =
  let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node
      ( 'n'
      , Node
          ('k', Node ('c', leaf 'a', Node ('e', leaf 'd', leaf 'g')), leaf 'm')
      , Node ('u', Node ('p', Empty, leaf 'q'), Empty) )
  in
  [%test_eq: (char * int * int) binary_tree]
    (layout_binary_tree_2 example_layout_tree)
    (Node
       ( ('n', 15, 1)
       , Node
           ( ('k', 7, 2)
           , Node
               ( ('c', 3, 3)
               , Node (('a', 1, 4), Empty, Empty)
               , Node
                   ( ('e', 5, 4)
                   , Node (('d', 4, 5), Empty, Empty)
                   , Node (('g', 6, 5), Empty, Empty) ) )
           , Node (('m', 11, 3), Empty, Empty) )
       , Node
           ( ('u', 23, 2)
           , Node (('p', 19, 3), Empty, Node (('q', 21, 4), Empty, Empty))
           , Empty ) ));
  let example2_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node ('n', Empty, Node ('u', Node ('p', Empty, leaf 'q'), Empty))
  in
  [%test_eq: (char * int * int) binary_tree]
    (layout_binary_tree_2 example2_layout_tree)
    (Node
       ( ('n', 1, 1)
       , Empty
       , Node
           ( ('u', 5, 2)
           , Node (('p', 3, 3), Empty, Node (('q', 4, 4), Empty, Empty))
           , Empty ) ))
;;

(* Problem 66 *)
let%test_unit "layout_binary_tree_3" =
  let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node
      ( 'n'
      , Node
          ('k', Node ('c', leaf 'a', Node ('e', leaf 'd', leaf 'g')), leaf 'm')
      , Node ('u', Node ('p', Empty, leaf 'q'), Empty) )
  in
  [%test_eq: (char * int * int) binary_tree]
    (layout_binary_tree_3 example_layout_tree)
    (Node
       ( ('n', 5, 1)
       , Node
           ( ('k', 3, 2)
           , Node
               ( ('c', 2, 3)
               , Node (('a', 1, 4), Empty, Empty)
               , Node
                   ( ('e', 3, 4)
                   , Node (('d', 2, 5), Empty, Empty)
                   , Node (('g', 4, 5), Empty, Empty) ) )
           , Node (('m', 4, 3), Empty, Empty) )
       , Node
           ( ('u', 7, 2)
           , Node (('p', 6, 3), Empty, Node (('q', 7, 4), Empty, Empty))
           , Empty ) ));
  let example3_layout_tree =
    Node
      ( 'a'
      , Node ('b', Empty, Node ('e', Empty, Node ('f', Empty, Empty)))
      , Node ('c', Empty, Node ('d', Node ('g', Empty, Empty), Empty)) )
  in
  [%test_eq: (char * int * int) binary_tree]
    (layout_binary_tree_3 example3_layout_tree)
    (Node
       ( ('a', 3, 1)
       , Node
           ( ('b', 1, 2)
           , Empty
           , Node (('e', 2, 3), Empty, Node (('f', 3, 4), Empty, Empty)) )
       , Node
           ( ('c', 5, 2)
           , Empty
           , Node (('d', 6, 3), Node (('g', 5, 4), Empty, Empty), Empty) ) ))
;;

(* Problem 67 *)
let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node
    ( 'a'
    , Node ('b', leaf 'd', leaf 'e')
    , Node ('c', Empty, Node ('f', leaf 'g', Empty)) )
;;

let%test_unit "string_of_tree" =
  [%test_eq: string] (string_of_tree example_layout_tree) "a(b(d,e),c(,f(g,)))"
;;

let%test_unit "tree_of_string" =
  [%test_eq: char binary_tree]
    (tree_of_string "a(b(d,e),c(,f(g,)))")
    example_layout_tree
;;
