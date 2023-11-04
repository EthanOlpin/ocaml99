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

(* Problem 34 *)
let phi x =
  let rec aux acc i =
    if i >= x
    then acc
    else (
      let acc = if coprime x i then acc + 1 else acc in
      aux acc (i + 1))
  in
  aux 0 1
;;

(* Problem 35 *)
let divmod a b =
  let quotient = a / b in
  let remainder = a - (quotient * b) in
  quotient, remainder
;;

let rec factors n =
  let rec aux i =
    if n = 1
    then []
    else (
      let q, r = divmod n i in
      if r = 0 then i :: factors q else aux (i + 1))
  in
  aux 2
;;

(* Problem 36 *)
let factors_multiplicity n =
  let rec aux factors multiplicity acc =
    match factors with
    | a :: (b :: _ as rest) when a = b -> aux rest (multiplicity + 1) acc
    | x :: rest -> aux rest 1 ((x, multiplicity) :: acc)
    | [] -> acc
  in
  List.rev (aux (factors n) 1 [])
;;

(* Problem 37 *)
let phi_improved n =
  factors_multiplicity n
  |> List.fold ~init:1 ~f:(fun acc (p, m) -> acc * (p - 1) * (p ** (m - 1)))
;;
