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

(* Problem 36 *)
let%test_unit "factors_multiplicity" =
  [%test_eq: (int * int) list] (factors_multiplicity 315) [ 3, 2; 5, 1; 7, 1 ]
;;

(* Problem 37 *)
let%test_unit "phi_improved" =
  [%test_eq: int] (phi_improved 10) 4;
  [%test_eq: int] (phi_improved 13) 12
;;

(* Problem 39 *)
let%test_unit "all_primes" =
  [%test_eq: int] (List.length (all_primes 2 7920)) 1000
;;

(* Problem 40 *)
let%test_unit "goldbach" = [%test_eq: int * int] (goldbach 28) (5, 23)

(* Problem 41 *)
let%test_unit "goldbach_list" =
  [%test_eq: (int * (int * int)) list]
    (goldbach_list 9 20)
    [ 10, (3, 7)
    ; 12, (5, 7)
    ; 14, (3, 11)
    ; 16, (3, 13)
    ; 18, (5, 13)
    ; 20, (3, 17)
    ]
;;

let%test_unit "goldbach_limit" =
  [%test_eq: (int * (int * int)) list]
    (goldbach_limit 1 2000 50)
    [ 992, (73, 919); 1382, (61, 1321); 1856, (67, 1789); 1928, (61, 1867) ]
;;
