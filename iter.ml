(* 고차 함수 iter를 작성하시오:
          iter : int * (int -> int) -> (int -> int)
   정의는 다음과 같다:
   iter(n,f)=f ◦···◦f
   n = 0이면, iter(n, f ) 은 항등함수(identify function)으로 정의한다. n > 0이면, iter(n,f)은 f를 n번 반복 적용하는 함수이다. 예를 들어,
   iter(n, fun x -> 2+x) 0 는 2×n를 계산한다. *)

open Printf;;

let rec iter (n, f) =
  let compose _f _g _x = _f (_g _x) in
  if n = 0 then fun x -> x
  else compose f (iter (n-1, f));;

let _ = printf "%d" (iter(5, fun x -> 2+x) 0);;