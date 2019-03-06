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

(* double double
   => double (double a)
   => (double a) ((double a) b)
   => (double a) (double a b)
   => double a (double a b)
   => a (a (double a b))
   => a (a (a (a b)))
   --- a := inc , b := 0 -------
   => inc (inc (inc (inc 0)))
   => 4
*)

(* double (double double)
   => (double double) ((double double) a)
   => (double (double b)) ((double double) a)
   => ((double b) ((double b) c)) ((double double) a)
   => ((double b) (double b c)) ((double double) a)
   => (double b (double b c)) ((double double) a)
   => (double b (b (b c))) ((double double) a)
   => (b (b (b (b c)))) ((double double) a)
   => (b (b (b (b c)))) ((double (double d)) a)
   => (b (b (b (b c)))) (((double d) ((double d) e)) a)
   => (b (b (b (b c)))) (((double d) (double d e)) a)
   => (b (b (b (b c)))) ((double d (double d e)) a)
   => (b (b (b (b c)))) ((double d (d (d e))) a)
   => (b (b (b (b c)))) ((d (d (d (d e)))) a)
   => (b (b (b (b c)))) (a (a (a (a e))))
   => (a (a (a (a e)))) ((a (a (a (a e)))) ((a (a (a (a e)))) ((a (a (a (a e)))) c)))
   => (a (a (a (a e)))) ((a (a (a (a e)))) ((a (a (a (a e)))) (a (a (a (a c))))))
   => (a (a (a (a e)))) ((a (a (a (a e)))) (a (a (a (a (a (a (a (a c)))))))))
   => (a (a (a (a e)))) (a (a (a (a (a (a (a (a (a (a (a (a c))))))))))))
   => a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a c)))))))))))))))
   --- a := inc , c := 0 -------
   => inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 0)))))))))))))))
   => 16
*)


let _ = printf "%d" (((double (double double)) inc) 5);;