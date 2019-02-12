open Printf;;

let rec reverse_helper l acc =
  match l with
  | [] -> acc
  | hd :: tl -> reverse_helper tl (hd :: acc);;

let reverse l =
  reverse_helper l [];;

let _ = List.iter (printf "%d, ") (reverse [1;2;3;4]);;
