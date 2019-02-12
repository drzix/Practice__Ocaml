(* 두 원소를 가지는 튜플의 리스트를 두 리스트로 분해하는 함수
   unzip을 작성하시오:
   unzip: (’a * ’b) list -> ’a list * ’b list
   예를 들어,
   unzip [(1,"one");(2,"two");(3,"three")]
   은 ([1;2;3],["one";"two";"three"])을 계산한다. *)

open Printf;;

let rec unzip_helper l acc =
  match l with
  | [] -> (List.rev (fst acc), List.rev (snd acc))
  | hd :: tl -> 
    match hd with
    | (x, y) ->
      unzip_helper tl ((x :: (fst acc)), (y :: (snd acc)));;

let unzip l =
  unzip_helper l ([], []);;

let _ =
  (fun (l1, l2) ->
     List.iter (printf "%d, ") l1;
     List.iter (printf "%s, ") l2)
    (unzip [(1,"one");(2,"two");(3,"three")]);;