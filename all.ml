(* fold_right을 이용하여 고차 함수 all을 작성하시오:
   all : (’a -> bool) -> ’a list -> bool
   all p l은 리스트 l의 모든 원소들이 함수 p의 값을 참으로
   만드는지 여부를 나타낸다. 예를 들어,
   all (fun x -> x > 5) [7;8;9]
   는 true를 계산한다. *)

open Printf;;

let all p l =
  List.fold_right (&&) (List.map p l) true;;

let _ = printf "%b" (all (fun x -> x > 8) [7;8;9]);;