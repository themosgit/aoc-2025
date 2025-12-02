let return_line input =
    Seq.of_dispenser (fun() -> In_channel.input_line input)

let parse_line line =
    match line.[0] with
    | 'R' -> (try int_of_string
        (String.sub line 1 (String.length line - 1)) with Failure _ -> 0)
    | 'L' -> (try -(int_of_string
        (String.sub line 1 (String.length line - 1))) with Failure _ -> 0)
    |  _ -> 0
