open Printf;;

let rec remove_first a l =
  match l with
  | [] -> raise (Failure "no element")
  | hd::tl -> if hd = a then tl else hd :: remove_first a tl;;

let _ = List.iter (printf "%d, ") (remove_first 1 [1;2;3]);;
