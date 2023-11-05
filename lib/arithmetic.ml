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

(* Problem 37 & Problem 38 *)
let phi_improved n =
  factors_multiplicity n
  |> List.fold ~init:1 ~f:(fun acc (p, m) -> acc * (p - 1) * (p ** (m - 1)))
;;

(* Problem 39 *)
let all_primes start stop =
  let rec aux n primes =
    if n >= stop
    then primes
    else (
      let primes = if is_prime n then n :: primes else primes in
      aux (n + 1) primes)
  in
  List.rev (aux start [])
;;

(* Problem 40 *)
let goldbach x =
  if x < 4
  then failwith "Called goldbach with a number less than 4"
  else (
    let rec aux n =
      if is_prime n && is_prime (x - n) then n, x - n else aux (n + 1)
    in
    aux 2)
;;

(* Problem 41 *)
let goldbach_list low high =
  (* Goldbach's conjecture only applies to even numbers greater than 2*)
  let low = max low 4 in
  if low >= high
  then []
  else (
    let rec aux n acc =
      if n > high then acc else aux (n + 2) ((n, goldbach n) :: acc)
    in
    List.rev (aux (if low % 2 = 0 then low else low + 1) []))
;;

let goldbach_limit low high limit =
  goldbach_list low high
  |> List.filter ~f:(fun (_, (a, b)) -> a > limit && b > limit)
;;
