open Printf;;

let rec permutation l acc =
  let rec pick _l n _acc =
    match _l with
    | [] -> List.rev _acc
    | hd :: tl ->
      if n = 0 then hd :: (List.rev _acc) @ tl
      else pick tl (n - 1) (hd :: _acc)
  in
  match l with
  | [] ->
    List.iter (printf "%d ") (List.rev acc);
    printf "\n"
  | hd :: tl ->
    permutation tl (hd :: acc);
    for i = 1 to List.length l - 1 do
      let next_perm = pick l i [] in
      permutation (List.tl next_perm) (List.hd next_perm :: acc)
    done

let _ = permutation [1; 2; 3; 4;5;6;7;8;9;10] [];;