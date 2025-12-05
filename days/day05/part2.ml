let file = "days/day05/input.txt"

let parse_range line =
    Scanf.sscanf line "%d-%d" (fun start telos -> (start, telos))

let solve () =
    In_channel.with_open_text file @@ fun input ->
    let rec read_ranges acc =
        match In_channel.input_line input with
        | None | Some "" -> List.sort (fun (a, _) (b, _) -> compare a b) acc
        | Some line -> read_ranges (parse_range line :: acc)
    in
    let ranges = read_ranges [] in

    let rec count_fresh ranges last_telos acc =
        match ranges with
        | [] -> acc
        | (start, telos) :: rest ->
            if start <= last_telos + 1 then
                count_fresh rest (max telos last_telos) (acc + max 0 (telos - last_telos))
            else
                count_fresh rest telos (acc + (telos - start + 1))
    in
    let fresh_count = count_fresh ranges min_int 0 in
    Printf.printf "Total fresh IDs: %d\n" fresh_count
