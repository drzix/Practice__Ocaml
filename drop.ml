(* 리스트 l과 정수 n을 받아서 l의 첫 n개 원소를 제외한 나머지
   리스트를 구하는 함수 drop을 작성하시오:
   drop : ’a list -> int -> ’a list
   예를 들어,
          drop [1;2;3;4;5] 2 = [3; 4; 5]
          drop [1;2] 3 = []
          drop ["C"; "Java"; "OCaml"] 2 = ["OCaml"] *)

open Printf;;

let rec drop l n =
  if n = 0 then l
  else
    match l with
    | [] -> []
    | hd :: tl -> drop tl (n - 1);;

let _ = List.iter (printf "%d, ") (drop [1;2;3;4;5] 2);;