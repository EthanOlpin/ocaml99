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

(* Problem 48 *)
let%test_unit "table" =
  [%test_eq: ((string * bool) list * bool) list]
    (table [ "a"; "b" ] (And (Var "a", Or (Var "a", Var "b"))))
    [ [ "a", true; "b", true ], true
    ; [ "a", true; "b", false ], true
    ; [ "a", false; "b", true ], false
    ; [ "a", false; "b", false ], false
    ];
  [%test_eq: ((string * bool) list * bool) list]
    (let a = Var "a"
     and b = Var "b"
     and c = Var "c" in
     table
       [ "a"; "b"; "c" ]
       (Or (And (a, Or (b, c)), Or (And (a, b), And (a, c)))))
    [ [ "a", true; "b", true; "c", true ], true
    ; [ "a", true; "b", true; "c", false ], true
    ; [ "a", true; "b", false; "c", true ], true
    ; [ "a", true; "b", false; "c", false ], false
    ; [ "a", false; "b", true; "c", true ], false
    ; [ "a", false; "b", true; "c", false ], false
    ; [ "a", false; "b", false; "c", true ], false
    ; [ "a", false; "b", false; "c", false ], false
    ]
;;

(* Problem 49 *)
let%test_unit "gray" =
  [%test_eq: string list] (gray 1) [ "0"; "1" ];
  [%test_eq: string list] (gray 2) [ "00"; "01"; "11"; "10" ];
  [%test_eq: string list]
    (gray 3)
    [ "000"; "001"; "011"; "010"; "110"; "111"; "101"; "100" ]
;;
