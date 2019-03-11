open Printf;;

(* Exercise 1 "씨그마" *)
type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

exception Input_error of string
let rec calculate e =
  let rec assign_x _e n =
    match _e with
    | X -> n
    | INT i -> INT i
    | REAL r -> REAL r
    | ADD (e1,e2) -> ADD (assign_x e1 n, assign_x e2 n)
    | SUB (e1,e2) -> SUB (assign_x e1 n, assign_x e2 n)
    | MUL (e1,e2) -> MUL (assign_x e1 n, assign_x e2 n)
    | DIV (e1,e2) -> DIV (assign_x e1 n, assign_x e2 n)
    | SIGMA (start,fin,e1) -> SIGMA (assign_x start n, assign_x fin n, assign_x e1 n)
    | INTEGRAL (start,fin,e1) -> INTEGRAL (assign_x start n, assign_x fin n, assign_x e1 n)
  in
  match e with
  | X -> raise (Input_error "X is not assigned")
  | INT i -> float_of_int i
  | REAL r -> r
  | ADD (e1,e2) -> calculate e1 +. calculate e2
  | SUB (e1,e2) -> calculate e1 -. calculate e2
  | MUL (e1,e2) -> calculate e1 *. calculate e2
  | DIV (e1,e2) -> calculate e1 /. calculate e2
  | SIGMA (start,fin,e1) -> (
      if calculate start > calculate fin then 0.0
      else calculate (assign_x e1 start) +. calculate (SIGMA (ADD (start, INT 1), fin, e1))
    )
  | INTEGRAL (start,fin,e1) -> (
      if calculate start > calculate fin then 0.0
      else calculate (MUL ((assign_x e1 start), REAL 0.1)) +. calculate (INTEGRAL (ADD (start, REAL 0.1), fin, e1))
    )


(* Exercise 2 "Mathemadiga" 편미분? *)
type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

let diff (e,difv) =
  let rec _diff _e =
    match _e with
    | CONST c -> CONST 0
    | VAR v -> if v = difv then CONST 1 else CONST 0
    | POWER (v,nsq) ->
      if v = difv then (
        if nsq = 0 then CONST 0
        else TIMES [CONST nsq; POWER (v,nsq-1)]
      ) else CONST 0
    | TIMES l ->
      (match l with
       | [] -> CONST 0
       | hd::tl -> SUM [TIMES ((_diff hd) :: tl); TIMES [hd; _diff (TIMES tl)]]
      )
    | SUM l ->
      (match l with
       | [] -> CONST 0
       | hd::tl -> SUM [_diff (hd); _diff (SUM tl)]
      )
  in
  let rec clean _e =
    match _e with
    | POWER (v,nsq) ->
      (match nsq with
       | 0 -> CONST 1
       | 1 -> VAR v
       | _ -> _e
      )
    | TIMES l ->
      (match l with
       | [] -> CONST 1
       | hd::tl ->
         let c_hd = clean hd in
         (match c_hd with
          | CONST 0 -> CONST 0
          | CONST 1 -> clean (TIMES tl)
          | TIMES hdl ->
            let c_tl = clean (TIMES tl) in
            (match c_tl with
             | CONST 0 -> CONST 0
             | CONST 1 -> c_hd
             | TIMES tll -> TIMES (hdl@tll)
             | _ -> TIMES (c_tl::hdl)
            )
          | _ ->
            let c_tl = clean (TIMES tl) in
            (match c_tl with
             | CONST 0 -> CONST 0
             | CONST 1 -> c_hd
             | TIMES tll -> TIMES (c_hd::tll)
             | _ -> TIMES [c_hd; c_tl]
            )
         )
      )
    | SUM l ->
      (match l with
       | [] -> CONST 0
       | hd::tl ->
         let c_hd = clean hd in
         (match c_hd with
          | CONST 0 -> clean (SUM tl)
          | SUM hdl ->
            let c_tl = clean (SUM tl) in
            (match c_tl with
             | CONST 0 -> c_hd
             | SUM tll -> SUM (hdl@tll)
             | _ -> SUM (c_tl::hdl)
            )
          | _ ->
            let c_tl = clean (SUM tl) in
            (match c_tl with
             | CONST 0 -> c_hd
             | SUM tll -> SUM (c_hd::tll)
             | _ -> SUM [c_hd;c_tl]
            )
         )
      )
    | _ -> _e
  in
  match e with
  | TIMES [] -> raise (Input_error "Empty TIMES")
  | _ -> clean (_diff e)


(* TEST *)
let _ = printf "%f" (calculate (X))

(* diff (TIMES [
    SUM [TIMES [VAR "a"; POWER ("x", 2)]; VAR "x"];
    SUM [TIMES [VAR "b"; VAR "x"]; CONST 1]
   ], "x");; 
   diff (TIMES [VAR "b"; VAR "x"], "x");; *)