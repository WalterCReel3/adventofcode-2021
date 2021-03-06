open Genlex

let command_lexer = make_lexer ["forward"; "down"; "up";];;

type command  =
    | CmdForward of int
    | CmdDown of int
    | CmdUp of int
;;

exception Parse_Exception

let rec parse_command token_stream =
    match (Stream.next token_stream, Stream.next token_stream) with
    | (Kwd "forward", Int n) -> CmdForward n
    | (Kwd "down", Int n) -> CmdDown n
    | (Kwd "up", Int n) -> CmdUp n
    | _ -> raise Parse_Exception
;;

let load_cmd_queue filename =
    let tokens = command_lexer (Stream.of_channel (open_in filename)) in
    let cmd_queue = Queue.create () in
    let rec add_command tks =
        let command = parse_command tokens in
        Queue.add command cmd_queue;
        try add_command tks
        with Stream.Failure -> cmd_queue
    in
    add_command tokens
;;

let rec naive_commands (h, d) command_queue =
    try
        match (Queue.take command_queue) with
        | CmdForward forward -> naive_commands (h + forward, d) command_queue
        | CmdDown down -> naive_commands (h, d + down) command_queue
        | CmdUp up -> naive_commands (h, d - up) command_queue
    with Queue.Empty -> (h, d)
;;

let rec nav_commands (h, d, a) command_queue =
    try
        match (Queue.take command_queue) with
        | CmdForward forward -> nav_commands (h + forward, d + (forward * a), a) command_queue
        | CmdDown down -> nav_commands (h, d, a + down) command_queue
        | CmdUp up -> nav_commands (h, d, a - up) command_queue
    with Queue.Empty -> (h, d)
;;

let part1 command_queue =
    naive_commands (0, 0) command_queue
;;

let part2 command_queue =
    nav_commands (0, 0, 0) command_queue
;;

let main () =
    let commands_part1 = load_cmd_queue "input.txt" in
    let commands_part2 = Queue.copy commands_part1 in
    let (h1, d1) = part1 commands_part1 in
    let (h2, d2) = part2 commands_part2 in
    print_string "Part 1\n";
    Printf.printf "Position (%d, %d)\n" h1 d1;
    Printf.printf "Key: %d\n" (h1 * d1);
    print_string "Part 2\n";
    Printf.printf "Position (%d, %d)\n" h2 d2;
    Printf.printf "Key: %d\n" (h2 * d2)
;;

main ()