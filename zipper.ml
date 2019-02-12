(* 두 리스트 a와 b를 순차적으로 결합하는 함수 zipper를
   작성하시오:
   zipper: int list -> int list -> int list
   순차적인 결합이란 리스트 a의 i번째 원소가 리스트 b의 i번째 원소 앞에 오는 것을 의미한다. 짝이 맞지 않는 원소들은 뒤에 순서대로 붙인다.
   # zipper [1;3;5] [2;4;6];;
   - : int list = [1; 2; 3; 4; 5; 6]
   # zipper [1;3] [2;4;6;8];;
   - : int list = [1; 2; 3; 4; 6; 8]
   # zipper [1;3;5;7] [2;4];;
   - : int list = [1; 2; 3; 4; 5; 7] *)

open Printf;;

let rec zipper_helper (l: int list) (r: int list) acc =
  match l with
  | [] -> (List.rev r) @ acc
  | hd :: tl -> zipper_helper r tl (hd :: acc);;

let zipper (l: int list) (r: int list) =
  List.rev (zipper_helper l r []);;

let _ = List.iter (printf "%d, ") (zipper [1;3] [2;4;6;8]);;