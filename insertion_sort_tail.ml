open Printf;;

let sort l =
  let rec insert_helper x sorted acc =
    match sorted with
    | [] -> acc @ [x]
    | hd :: tl ->
      if x <= hd then acc @ x :: sorted
      else insert_helper x tl (acc @ [hd]) in

  let insert x sorted =
    insert_helper x sorted [] in

  let rec sort_helper l acc =
    match l with
    | [] -> acc
    | hd :: tl -> sort_helper tl (insert hd acc) in

  sort_helper l []
