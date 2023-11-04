open Base

(* Problem 31 *)
let is_prime x =
  let rec aux i = i * i > x || (x % i <> 0 && aux (i + 1)) in
  x >= 2 && aux 2
;;
