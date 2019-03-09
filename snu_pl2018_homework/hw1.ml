open Printf;;

(* Exercise 1 "씨그마" *)
let rec sigma (a,b,f) =
  if a > b then 0
  else f a + sigma (a+1, b, f)


(* Exercise 2 "합곱" *)
let sumprod (m,n,k) =
  let rec sum i =
    let rec prod j =
      if j > k then 1.0
      else m (i,j) *. prod (j+1) in
    if i > n then 0.0
    else prod 1 +. sum (i+1)
  in
  sum 1


(* Exercise 3 "대진표 스트링" *)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina
type tourna = LEAF of team
            | NODE of tourna * tourna

let string_of_team t =
  match t with
  | Korea -> "Korea"
  | France -> "France"
  | Usa -> "Usa"
  | Brazil -> "Brazil"
  | Japan -> "Japan"
  | Nigeria -> "Nigeria"
  | Cameroon -> "Cameroon"
  | Poland -> "Poland"
  | Portugal -> "Portugal"
  | Italy -> "Italy"
  | Germany -> "Germany"
  | Norway -> "Norway"
  | Sweden -> "Sweden"
  | England -> "England"
  | Argentina -> "Argentina"

let rec parenize tn = 
  match tn with
  | NODE (l,r) -> "(" ^ parenize l ^ " " ^ parenize r ^ ")"
  | LEAF t -> string_of_team t


(* Exercise 4 "참거짓" *)
type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
and expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr

let rec eval form =
  match form with
  | TRUE -> true
  | FALSE -> false
  | NOT f -> not (eval f)
  | ANDALSO (f1,f2) -> eval f1 && eval f2
  | ORELSE (f1,f2) -> eval f1 || eval f2
  | IMPLY (f1,f2) -> not (eval f1) || eval f2
  | LESS (e1,e2) -> parse e1 < parse e2
and parse ex =
  match ex with
  | NUM n -> n
  | PLUS (e1,e2) -> parse e1 + parse e2
  | MINUS (e1,e2) -> parse e1 - parse e2


(* Exercise 5 "자연수" *)
type nat = ZERO
         | SUCC of nat

let rec natadd (x,y) =
  match x with
  | ZERO -> y
  | SUCC n -> natadd (n, SUCC y)
and natmul (x,y) =
  match x with
  | ZERO -> ZERO
  | SUCC ZERO -> y
  | SUCC n -> natadd (y, natmul (n,y))

let _ = printf "%b" (eval (ORELSE((LESS(NUM 8, NUM 4)), LESS(NUM 3, PLUS(NUM 1, NUM 3)))))