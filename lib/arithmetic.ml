open Base

(* Problem 31 *)
let is_prime x =
  let rec aux i = i * i > x || (x % i <> 0 && aux (i + 1)) in
  x >= 2 && aux 2
;;

(* Problem 32 *)
let rec gcd a b = if b = 0 then a else gcd b (a % b)

(* Problem 33 *)
let coprime a b = gcd a b = 1

