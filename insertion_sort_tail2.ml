open Printf;;

let rec range_helper n m acc =
  if n > m then acc
  else range_helper (n + 1) m (n :: acc);;

let range n m =
  List.rev (range_helper n m []);;

let sort l =
  let insert sorted x =
    let rec insert_helper sorted x acc =
      match sorted with
      | [] -> List.rev (x :: acc)
      | hd :: tl ->
        if x <= hd then List.rev acc @ x :: sorted
        else insert_helper tl x (hd :: acc)
    in
    insert_helper sorted x []
  in
  List.fold_left (insert) [] l;;

let _ = List.iter (printf "%d, ") (sort (range 0 10000));;