open Base
open Ocaml99.Multiway_trees

(* Problem 70C *)
let%test_unit "count_nodes" =
  [%test_eq: int] (count_nodes (T ('a', [ T ('f', []) ]))) 2
;;
