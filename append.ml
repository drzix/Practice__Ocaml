open Printf;;

let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | h::t -> h :: (append t l2);;

let _ = List.iter (printf "%d, ") (append [1;2] [3;4]);;
