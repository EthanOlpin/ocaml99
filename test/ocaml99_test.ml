open Base
open Ocaml99

(* Problem 01 *)
let%test_unit "last" =
  [%test_eq: string option] (last [ "a"; "b"; "c"; "d" ]) (Some "d");
  [%test_eq: string option] (last []) None
;;

(* Problem 02 *)
let%test_unit "last_two" =
  [%test_eq: (string * string) option]
    (last_two [ "a"; "b"; "c"; "d" ])
    (Some ("c", "d"));
  [%test_eq: (string * string) option] (last_two [ "a" ]) None
;;

(* Problem 03 *)
let%test_unit "at" =
  [%test_eq: string option] (at 3 [ "a"; "b"; "c"; "d"; "e" ]) (Some "c");
  [%test_eq: string option] (at 3 [ "a" ]) None
;;

(* Problem 04 *)
let%test_unit "length" =
  [%test_eq: int] (length [ "a"; "b"; "c" ]) 3;
  [%test_eq: int] (length []) 0
;;

(* Problem 05 *)
let%test_unit "rev" =
  [%test_eq: string list] (rev [ "a"; "b"; "c" ]) [ "c"; "b"; "a" ]
;;

(* Problem 06 *)
let%test_unit "is_palindrome" =
  [%test_eq: bool] (is_palindrome [ "x"; "a"; "m"; "a"; "x" ]) true;
  [%test_eq: bool] (is_palindrome [ "a"; "b" ]) false
;;

(* Problem 07 *)
let%test_unit "flatten" =
  [%test_eq: string list]
    (flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ])
    [ "a"; "b"; "c"; "d"; "e" ]
;;

(* Problem 08 *)
let%test_unit "compress" =
  [%test_eq: string list]
    (compress
       [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
    [ "a"; "b"; "c"; "a"; "d"; "e" ]
;;

(* Problem 09 *)
let%test_unit "pack" =
  [%test_eq: string list list]
    (pack
       [ "a"
       ; "a"
       ; "a"
       ; "a"
       ; "b"
       ; "c"
       ; "c"
       ; "a"
       ; "a"
       ; "d"
       ; "d"
       ; "e"
       ; "e"
       ; "e"
       ; "e"
       ])
    [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ]
;;

(* Problem 10 & Problem 13 *)
let%test_unit "encode" =
  [%test_eq: (int * string) list]
    (encode
       [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
    [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ]
;;

(* Problem 11 *)
let%test_unit "modified_encode" =
  [%test_eq: string rle list]
    (modified_encode
       [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
    [ Many (4, "a")
    ; One "b"
    ; Many (2, "c")
    ; Many (2, "a")
    ; One "d"
    ; Many (4, "e")
    ]
;;

(* Problem 12 *)
let%test_unit "decode" =
  [%test_eq: string list]
    (decode
       [ Many (4, "a")
       ; One "b"
       ; Many (2, "c")
       ; Many (2, "a")
       ; One "d"
       ; Many (4, "e")
       ])
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
;;
