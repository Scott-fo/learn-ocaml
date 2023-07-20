let rec last = function
    | [] -> None
    | [ x ] -> Some x
    | _ :: t -> last t;;

let%test "Get tail of list" = last ["a" ; "b" ; "c" ; "d"] = Some "d"
