open Base

type 'a mult_tree = T of 'a * 'a mult_tree list

(* Problem 70C *)
let rec count_nodes = function
  | T (_, []) -> 1
  | T (_, rest) ->
    1 + List.fold rest ~init:0 ~f:(fun acc t -> acc + count_nodes t)
;;
