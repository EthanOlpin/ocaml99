open Base

(* Problem 01 *)
let rec last l =
  match l with
  | [] -> None
  | [ x ] -> Some x
  | _ :: rest -> last rest
;;
