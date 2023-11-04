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

(* Problem 33 *)
let%test_unit "coprime" =
  [%test_eq: bool] (coprime 13 27) true;
  [%test_eq: bool] (coprime 20536 7826) false
;;

(* Problem 34 *)
let%test_unit "phi" =
  [%test_eq: int] (phi 10) 4;
  [%test_eq: int] (phi 13) 12
;;

(* Problem 35 *)
let%test_unit "factors" = [%test_eq: int list] (factors 315) [ 3; 3; 5; 7 ]

