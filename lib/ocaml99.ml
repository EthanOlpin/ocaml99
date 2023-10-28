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
