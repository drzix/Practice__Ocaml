(* 고차 함수 sigma를 작성하시오:
   sigma : (int -> int) -> int -> int -> int sigma f a b는 다음을 계산한다.
   예를 들어,
   sigma (fun x -> x) 1 10
   의 값은 55이고,
   sigma (fun x -> x*x) 1 7
   는 140을 계산한다. *)

open Printf;;

let rec sigma f a b =
  if a > b then 0
  else f a + sigma f (a + 1) b;;

let _ = printf "%d" (sigma (fun x -> x*x) 1 7);;