(* 1) *)
let rec last l =
  match l with
  | [] -> None
  | [ xs ] -> Some xs
  | _ :: tl -> last tl
;;

assert (last [ "a"; "b"; "c"; "d" ] = Some "d");
assert (last [] = None)

(* 2) *)
let rec last_two l =
  match l with
  | [] | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: tl -> last_two tl
;;

assert (last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d"));
assert (last_two [ "a" ] = None)

(* 3) *)
let rec at i l =
  match i, l with
  | _, [] -> None
  | 0, hd :: _ -> Some hd
  | x, _ :: tl -> at (x - 1) tl
;;

assert (at 2 [ "a"; "b"; "c"; "d"; "e" ] = Some "c");
assert (at 2 [ "a" ] = None)

(* 4) *)
let length l =
  let rec loop n l =
    match l with
    | [] -> n
    | _ :: tl -> loop (n + 1) tl
  in
  loop 0 l
;;

assert (length [ "a"; "b"; "c" ] = 3);
assert (length [] = 0)

(* 5) *)
let rec rev l =
  match l with
  | [] -> []
  | hd :: tl -> rev tl @ [ hd ]
;;

assert (rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ])

(* 6) *)
let is_palindrome l = l = rev l;;

assert (is_palindrome [ "x"; "a"; "m"; "a"; "x" ]);
assert (not (is_palindrome [ "a"; "b" ]))

type 'a node =
  | One of 'a
  | Many of 'a node list

(* 7) *)
let flatten l =
  let rec loop acc ln =
    match ln with
    | [] -> acc
    | One x :: tl -> loop (acc @ [ x ]) tl
    | Many ll :: tl -> loop (loop acc ll) tl
  in
  loop [] l
;;

assert (
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  = [ "a"; "b"; "c"; "d"; "e" ])

(* 8) *)
let compress l =
  let rec aux acc l =
    match l, acc with
    | [], a -> a
    | [ x ], [] -> [ x ]
    | [ x ], a0 :: _ -> if x = a0 then acc else x :: acc
    | hd :: tl, acc -> aux (aux acc [ hd ]) tl
  in
  rev @@ aux [] l
;;

assert (
  compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ "a"; "b"; "c"; "a"; "d"; "e" ])

(* 9) *)
let pack l =
  let rec aux acc l =
    match l, acc with
    | [], a -> a
    | [ x ], [] -> [ [ x ] ]
    | [ x ], (ax :: atl) :: ll -> if x = ax then (x :: ax :: atl) :: ll else [ x ] :: acc
    | hd :: tl, acc -> aux (aux acc [ hd ]) tl
  in
  rev @@ aux [] l
;;

assert (
  pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
  = [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ])

(* 10) *)
let encode l =
  let rec aux acc l =
    match l, acc with
    | [], [] -> []
    | [], a -> a
    | [ x ], [] -> [ 1, x ]
    | [ x ], (n, ax) :: atl -> if x = ax then (n + 1, ax) :: atl else (1, x) :: acc
    | hd :: tl, acc -> aux (aux acc [ hd ]) tl
  in
  rev @@ aux [] l
;;

assert (
  encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ])
