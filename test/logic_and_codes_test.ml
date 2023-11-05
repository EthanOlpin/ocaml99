open Base
open Ocaml99.Logic_and_codes

(* Problem 46 & Problem 47 *)
let%test_unit "table2" =
  [%test_eq: (bool * bool * bool) list]
    (table2 "a" "b" (And (Var "a", Or (Var "a", Var "b"))))
    [ true, true, true
    ; true, false, true
    ; false, true, false
    ; false, false, false
    ]
;;
