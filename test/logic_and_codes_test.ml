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

(* Problem 50 *)
let%test_unit "MinHeap" =
  let heap = MinHeap.create ~len:1 0 Int.compare in
  MinHeap.push heap 5;
  MinHeap.push heap 3;
  MinHeap.push heap 4;
  [%test_eq: int] (MinHeap.pop heap) 3;
  [%test_eq: int] (MinHeap.pop heap) 4;
  [%test_eq: int] (MinHeap.pop heap) 5
;;

let%test_unit "huffman" =
  [%test_eq: (string * string) list]
    (huffman [ "a", 45; "b", 13; "c", 12; "d", 16; "e", 9; "f", 5 ])
    [ "a", "0"; "c", "100"; "b", "101"; "f", "1100"; "e", "1101"; "d", "111" ];
  [%test_eq: (string * string) list]
    (huffman [ "a", 10; "b", 15; "c", 30; "d", 16; "e", 29 ])
    [ "d", "00"; "a", "010"; "b", "011"; "e", "10"; "c", "11" ]
;;
