open Base
open Ocaml99

(* Problem 01 *)
let%test_unit "last" =
  [%test_eq: string option] (last [ "a"; "b"; "c"; "d" ]) (Some "d");
  [%test_eq: string option] (last []) None
;;

(* Problem 02 *)
let%test_unit "last_two" =
  [%test_eq: (string * string) option]
    (last_two [ "a"; "b"; "c"; "d" ])
    (Some ("c", "d"));
  [%test_eq: (string * string) option] (last_two [ "a" ]) None
;;
