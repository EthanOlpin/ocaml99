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
