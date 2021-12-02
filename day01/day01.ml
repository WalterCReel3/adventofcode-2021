let load_input filename =
    let input_channel = open_in filename in 
    let rec read_lines infile entries =
        try read_lines infile ((int_of_string (input_line infile)) :: entries)
        with End_of_file -> entries
    in
    read_lines input_channel []
;;

let binary_fold_left f init input_list = 
    let rec inner f v l =
        match l with
        | lh::rh::tl -> inner f (f v (lh, rh)) (rh::tl)
        | _ -> v
    in
    inner f init input_list 
;;

let rec ternary_map f input_list =
    match input_list with
    | t1::t2::t3::tl -> ((f t1 t2 t3) :: (ternary_map f (t2::t3::tl)))
    | _ -> []
;;

let part1 l =
    binary_fold_left (fun acc (lh, rh) -> (if lh > rh then 1 else 0) + acc) 0 l
;;

let part2 l =
    binary_fold_left (fun acc (lh, rh) -> (if lh > rh then 1 else 0) + acc) 0 (ternary_map (fun t1 t2 t3 -> t1 + t2 + t3) l)
;;

let input = load_input "input.txt" in
    Printf.printf "Part 1: %d\n" (part1 input);
    Printf.printf "Part 2: %d\n" (part2 input)
