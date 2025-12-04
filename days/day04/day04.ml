let run part =
    match part with
    | Some 1 -> Part1.solve ()
    | Some 2 -> Part2.solve ()
    | Some n -> Printf.eprintf "Error: %d is an invalid part number\n" n
    | None -> Part1.solve () ; Part2.solve ()
