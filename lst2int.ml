(* fold_left를 이용하여 정수 리스트를 숫자로 변환하는 함수를
   작성하시오:
                  lst2int : int list -> int
   예를 들어, lst2int [1;2;3]는 123을 계산한다. 리스트의 원소들은 0이상 9이하의 수라고 가정한다. *)

open Printf;;

let lst2int l =
  List.fold_left (fun acc x -> acc ^ string_of_int x) "" l;;

let _ = printf "%s" (lst2int [1;2;3]);;