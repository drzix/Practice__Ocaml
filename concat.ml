(* 리스트의 리스트를 받아서 모든 원소를 포함하는 하나의 리스트를
   반환하는 함수 concat을 작성하시오:
   concat: ’a list list -> ’a list
   예를 들어,
   concat [[1;2];[3;4;5]] = [1;2;3;4;5] *)

open Printf;;

let rec concat_helper l acc =
  match l with
  | [] -> acc
  | hd :: tl -> concat_helper tl (acc @ hd);;

let concat l =
  concat_helper l []

let _ = List.iter (printf "%d, ") (concat [[1;2];[3;4;5]]);;