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

(* Problem 03 *)
let%test_unit "at" =
  [%test_eq: string option] (at 3 [ "a"; "b"; "c"; "d"; "e" ]) (Some "c");
  [%test_eq: string option] (at 3 [ "a" ]) None
;;

(* Problem 04 *)
let%test_unit "length" =
  [%test_eq: int] (length [ "a"; "b"; "c" ]) 3;
  [%test_eq: int] (length []) 0
;;

(* Problem 05 *)
let%test_unit "rev" =
  [%test_eq: string list] (rev [ "a"; "b"; "c" ]) [ "c"; "b"; "a" ]
;;
