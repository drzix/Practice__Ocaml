(* 두 수 n, m (n ≤ m)을 받아서 n이상 m이하의 수로 구성된
   리스트를 반환하는 함수 range를 작성하시오: range : int -> int -> int list
   예를 들어, range 3 7 는 [3;4;5;6;7]를 계산한다. *)

open Printf;;

let rec range_helper n m acc =
  if n > m then acc
  else range_helper (n + 1) m (n :: acc);;

let range n m =
  List.rev (range_helper n m []);;

let _ = List.iter (printf "%d, ") (range 3 700000);;