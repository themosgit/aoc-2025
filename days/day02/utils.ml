let pair a b = (a, b)
let parse_range input =
    let scanner = Scanf.Scanning.from_channel input in
    Seq.of_dispenser @@ fun() ->
    try let (num1, num2) = Scanf.bscanf scanner " %d-%d" pair in
        if not (Scanf.Scanning .end_of_input scanner) then
            Scanf.bscanf scanner "%c" ignore;
    Some (num1, num2)
    with End_of_file -> None


