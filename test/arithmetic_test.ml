open Base
open Ocaml99.Arithmetic

(* Problem 31 *)
let%test_unit "is_prime" =
  [%test_eq: bool] (is_prime 1) false;
  [%test_eq: bool] (is_prime 7) true;
  [%test_eq: bool] (is_prime 12) false
;;
