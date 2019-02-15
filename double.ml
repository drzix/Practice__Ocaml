(* 함수 f 와 인자 a를 받아서 f 를 a에 두 번 적용한 결과를 반환하는 함수
   double을 작성하시오:
   double: (’a -> ’a) -> ’a -> ’a
   예를 들어,
    # let inc x = x + 1;;
    val inc : int -> int = <fun>
    # let mul x = x * 2;;
    val mul : int -> int = <fun>
    # (double inc) 1;;
    - : int = 3
    # (double mul) 1;;
    - : int = 4
   아래 식의 값은?
   ((double double) inc) 0
   (double double) mul 2
   ((double (double double)) inc) 5 *)

open Printf;;

let inc x = x + 1;;
let mul x = x * 2;;

let double f a = f (f a);;

let _ = printf "%d" (((double (double double)) inc) 5);;