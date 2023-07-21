let rec last = function [] -> None | [ x ] -> Some x | _ :: t -> last t
let%test "Get tail of list" = last [ "a"; "b"; "c"; "d" ] = Some "d"
let%test "Get tail of list , invalid" = last [] = None

let rec last_two = function
  | [] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t

let%test "Get last two items of list" =
  last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d")

let%test "Get last two items of list, invalid" = last_two [ "a" ] = None

let rec at k = function
  | [] -> None
  | h :: t -> if k = 0 then Some h else at (k - 1) t

let%test "Get Nth element of list" = at 2 [ "a"; "b"; "c"; "d" ] = Some "c"
let%test "Get Nth element of list, invalid" = at 2 [ "a" ] = None

let length list =
  let rec aux n = function [] -> n | _ :: t -> aux (n + 1) t in
  aux 0 list

let%test "Get length of list" = length [ "a"; "b"; "c" ] = 3
let%test "Get length of list, empty" = length [] = 0

let rev list =
  let rec aux acc = function [] -> acc | h :: t -> aux (h :: acc) t in
  aux [] list

let%test "Get length of list" = rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]
let is_palindrome list = list = List.rev list

let%test "Check if list is a palindrome" =
  is_palindrome [ "x"; "a"; "m"; "a"; "x" ]

type 'a node = One of 'a | Many of 'a node list

let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  List.rev (aux [] list)

let%test "Flatten list" =
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  = [ "a"; "b"; "c"; "d"; "e" ]

let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller

let%test "Remove duplicates from a list" =
  compress
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ "a"; "b"; "c"; "a"; "d"; "e" ]

let pack list =
  let rec aux current acc = function
    | [] -> []
    | [ x ] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (a :: current) acc t
        else aux [] ((a :: current) :: acc) t
  in
  List.rev (aux [] [] list)

let%test "Pack consecutive duplicates" =
  pack
    [
      "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e";
    ]
  = [
      [ "a"; "a"; "a"; "a" ];
      [ "b" ];
      [ "c"; "c" ];
      [ "a"; "a" ];
      [ "d"; "d" ];
      [ "e"; "e"; "e"; "e" ];
    ]
