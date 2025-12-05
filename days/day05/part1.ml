let file = "days/day05/input.txt"

let parse_range line =
    Scanf.sscanf line "%d-%d" (fun start telos -> (start, telos))

let is_fresh ranges id =
    List.exists (fun (start, telos) -> id >= start && id <= telos) ranges

let solve () =
    In_channel.with_open_text file @@ fun input ->
    let rec read_ranges acc =
        match In_channel.input_line input with
        | None | Some "" -> List.rev acc
        | Some line -> read_ranges (parse_range line :: acc)
    in
    let ranges = read_ranges [] in

    let fresh_count =
        Seq.of_dispenser (fun () -> In_channel.input_line input)
        |> Seq.map int_of_string
        |> Seq.filter (is_fresh ranges)
        |> Seq.fold_left (fun acc _ -> acc + 1) 0
    in
    Printf.printf "Number of fresh ingredients: %d\n" fresh_count
