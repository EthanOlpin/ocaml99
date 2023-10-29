open Base

(* Problem 01 *)
let rec last l =
  match l with
  | [] -> None
  | [ x ] -> Some x
  | _ :: rest -> last rest
;;

(* Problem 02 *)
let rec last_two l =
  match l with
  | [] | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: rest -> last_two rest
;;

(* Problem 03 *)
let rec at i l =
  match l with
  | _ :: rest when i > 1 -> at (i - 1) rest
  | x :: _ when i = 1 -> Some x
  | _ -> None
;;

(* Problem 04 *)
let length l =
  let rec aux l len =
    match l with
    | [] -> len
    | _ :: rest -> aux rest (len + 1)
  in
  aux l 0
;;

(* Problem 05 *)
let rev l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | x :: rest -> aux rest (x :: acc)
  in
  aux l []
;;

(* Problem 06 *)
let rec list_eq a b =
  match a, b with
  | [ x ], [ y ] when Poly.(x = y) -> true
  | x :: rest_a, y :: rest_b when Poly.(x = y) -> list_eq rest_a rest_b
  | _ -> false
;;

let is_palindrome l = list_eq l (rev l)

(* Problem 07 *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten n =
  let rec aux n acc =
    match n with
    | One x :: rest -> aux rest (x :: acc)
    | Many l :: rest -> aux rest (aux l acc)
    | _ -> acc
  in
  aux n [] |> rev
;;

(* Problem 08 *)
let rec compress l =
  match l with
  | a :: b :: rest ->
    if Poly.(a = b) then compress (a :: rest) else a :: compress (b :: rest)
  | l -> l
;;

(* Problem 09 *)
let pack l =
  let rec aux l acc =
    match l, acc with
    | a :: rest_a, (b :: rest_b) :: rest_acc when Poly.(a = b) ->
      aux rest_a ((a :: b :: rest_b) :: rest_acc)
    | a :: rest_a, _ -> aux rest_a ([ a ] :: acc)
    | [], _ -> acc
  in
  aux l [] |> rev
;;

(* Problem 10 & Problem 13 *)
let encode l =
  let rec aux l acc curr curr_count =
    match l with
    | x :: rest when Poly.(x = curr) -> aux rest acc curr (curr_count + 1)
    | x :: rest -> aux rest ((curr_count, curr) :: acc) x 1
    | [] -> (curr_count, curr) :: acc
  in
  match l with
  | x :: rest -> aux rest [] x 1 |> rev
  | [] -> []
;;

(* Problem 11 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a
[@@deriving sexp, compare]

let modified_encode l =
  let to_rle el count = if count > 1 then Many (count, el) else One el in
  let rec aux l acc curr curr_count =
    match l with
    | x :: rest when Poly.(x = curr) -> aux rest acc curr (curr_count + 1)
    | x :: rest -> aux rest (to_rle curr curr_count :: acc) x 1
    | [] -> to_rle curr curr_count :: acc
  in
  match l with
  | x :: rest -> aux rest [] x 1 |> rev
  | [] -> []
;;

(* Problem 12 *)
let decode l =
  let rec expand el count acc =
    if count = 0 then acc else expand el (count - 1) (el :: acc)
  in
  let rec aux l acc =
    match l with
    | One x :: rest -> aux rest (x :: acc)
    | Many (count, x) :: rest -> aux rest (expand x count acc)
    | [] -> acc
  in
  aux l [] |> rev
;;

(* Problem 14 *)
let rec duplicate l =
  match l with
  | a :: rest -> a :: a :: duplicate rest
  | [] -> []
;;

(* Problem 15 *)
let replicate l count =
  let rec clone el count acc =
    if count = 0 then acc else clone el (count - 1) (el :: acc)
  in
  let rec aux l acc =
    match l with
    | [] -> acc
    | x :: rest -> aux rest (clone x count acc)
  in
  match count with
  | 0 -> []
  | 1 -> l
  | _ -> aux l [] |> rev
;;

(* Problem 16 *)
let drop l n =
  let rec aux l acc left =
    match l, left with
    | [], _ -> acc
    | _ :: rest, 1 -> aux rest acc n
    | x :: rest, left -> aux rest (x :: acc) (left - 1)
  in
  aux l [] n |> rev
;;

(* Problem 17 *)
let split l len =
  let rec aux l acc len =
    match l, len with
    | [], _ | _, 0 -> acc |> rev, l
    | x :: rest, rem_len -> aux rest (x :: acc) (rem_len - 1)
  in
  aux l [] len
;;

(* Problem 18 *)
let slice l s e =
  let _, right = split l s in
  let left, _ = split right (e - s + 1) in
  left
;;

(* Problem 19 *)
let rec concat a b =
  match a with
  | [] -> b
  | x :: rest -> x :: concat rest b
;;

let rec rotate l n =
  if n >= 0
  then (
    let left, right = split l n in
    concat right left)
  else rev (rotate (rev l) (-1 * n))
;;

(* Problem 20 *)
let rec remove_at i l =
  match l with
  | _ :: rest when i = 0 -> rest
  | x :: rest -> x :: remove_at (i - 1) rest
  | [] -> []
;;

(* Problem 21 *)
let rec insert_at el i l =
  if i = 0
  then (
    match l with
    | x :: rest -> el :: x :: rest
    | [] -> [ el ])
  else (
    match l with
    | x :: rest -> x :: insert_at el (i - 1) rest
    | [] -> [])
;;

(* Problem 22 *)
let range start last =
  let dir = if start > last then -1 else 1 in
  let rec aux start last acc =
    if start = last then last :: acc else aux (start + dir) last (start :: acc)
  in
  rev (aux start last [])
;;

(* Problem 23 *)
let rand_select l n =
  let rand_pop l =
    let rec pop_at l i left =
      match l, i with
      | [], _ -> failwith "could not pop random value from list"
      | x :: rest, 0 -> x, concat (rev left) rest
      | x :: rest, _ -> pop_at rest (i - 1) (x :: left)
    in
    pop_at l (Random.int (length l)) []
  in
  let rec aux l acc n =
    match l, n with
    | _, 0 -> acc
    | [], _ -> failwith "did not find enough elements in list"
    | l, _ ->
      let el, rest = rand_pop l in
      aux rest (el :: acc) (n - 1)
  in
  aux l [] n
;;

(* Problem 24 *)
let lotto_select n max =
  let rng = range 1 max in
  rand_select rng n
;;

(* Problem 25 *)
let permutation l = rand_select l (length l)

(* Problem 26 *)
let rec extract n l =
  let prepend_all el ls =
    let rec aux el ls acc =
      match ls with
      | x :: rest -> aux el rest ((el :: x) :: acc)
      | [] -> rev acc
    in
    aux el ls []
  in
  let rec aux n l acc =
    match n, l with
    | 0, _ -> [ [] ]
    | _, [] -> acc
    | _, x :: rest ->
      let combs = prepend_all x (extract (n - 1) rest) in
      aux n rest (concat acc combs)
  in
  aux n l []
;;
