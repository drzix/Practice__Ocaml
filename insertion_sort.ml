open Printf;;

let rec range_helper n m acc =
  if n > m then acc
  else range_helper (n + 1) m (n :: acc);;

let range n m =
  List.rev (range_helper n m []);;

let rec sort l =
  let rec insert elem sorted =
    match sorted with
    | [] -> elem :: sorted
    | hd :: tl -> if elem <= hd then elem :: sorted
      else hd :: (insert elem tl) in
  match l with
  | [] -> []
  | hd :: tl -> insert hd (sort tl)

let _ = List.iter (printf "%d, ") (sort (List.rev (range 0 10000)));;