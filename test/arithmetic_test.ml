open Base
open Ocaml99.Arithmetic

(* Problem 31 *)
let%test_unit "is_prime" =
  [%test_eq: bool] (is_prime 1) false;
  [%test_eq: bool] (is_prime 7) true;
  [%test_eq: bool] (is_prime 12) false
;;

(* Problem 32 *)
let%test_unit "gcd" =
  [%test_eq: int] (gcd 13 27) 1;
  [%test_eq: int] (gcd 20536 7826) 2
;;
