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

(* Problem 14 *)
let%test_unit "duplicate" =
  [%test_eq: string list]
    (duplicate [ "a"; "b"; "c"; "c"; "d" ])
    [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]
;;

(* Problem 15 *)
let%test_unit "replicate" =
  [%test_eq: string list]
    (replicate [ "a"; "b"; "c" ] 3)
    [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
;;

(* Problem 16 *)
let%test_unit "drop" =
  [%test_eq: string list]
    (drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3)
    [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]
;;

(* Problem 17 *)
let%test_unit "split" =
  [%test_eq: string list * string list]
    (split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3)
    ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]);
  [%test_eq: string list * string list]
    (split [ "a"; "b"; "c"; "d" ] 5)
    ([ "a"; "b"; "c"; "d" ], [])
;;

(* Problem 18 *)
let%test_unit "slice" =
  [%test_eq: string list]
    (slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6)
    [ "c"; "d"; "e"; "f"; "g" ]
;;

(* Problem 19 *)
let%test_unit "rotate" =
  [%test_eq: string list]
    (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3)
    [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ];
  [%test_eq: string list]
    (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-2))
    [ "g"; "h"; "a"; "b"; "c"; "d"; "e"; "f" ]
;;

(* Problem 20 *)
let%test_unit "remove_at" =
  [%test_eq: string list] (remove_at 1 [ "a"; "b"; "c"; "d" ]) [ "a"; "c"; "d" ]
;;

(* Problem 21 *)
let%test_unit "insert_at" =
  [%test_eq: string list]
    (insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ])
    [ "a"; "alfa"; "b"; "c"; "d" ];
  [%test_eq: string list]
    (insert_at "alfa" 3 [ "a"; "b"; "c"; "d" ])
    [ "a"; "b"; "c"; "alfa"; "d" ];
  [%test_eq: string list]
    (insert_at "alfa" 4 [ "a"; "b"; "c"; "d" ])
    [ "a"; "b"; "c"; "d"; "alfa" ]
;;

(* Problem 22 *)
let%test_unit "range" =
  [%test_eq: int list] (range 4 9) [ 4; 5; 6; 7; 8; 9 ];
  [%test_eq: int list] (range 9 4) [ 9; 8; 7; 6; 5; 4 ]
;;

(* Problem 23 *)
let%test_unit "rand_select" =
  [%test_eq: string list]
    (rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3)
    [ "b"; "c"; "d" ]
;;

(* Problem 24 *)
let%test_unit "lotto_select" =
  [%test_eq: int list] (lotto_select 6 49) [ 41; 7; 8; 17; 26; 5 ]
;;
;;
