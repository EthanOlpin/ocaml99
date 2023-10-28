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
