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
