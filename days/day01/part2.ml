let file = "days/day01/inputs/part2.txt"
(* utils functions in utils.ml *)
let solve () =
    In_channel.with_open_text file @@ fun input ->
    let (_idx, zeros) =
        Utils.return_line input
        (* lazy load each line compute from left *)
        |> Seq.map Utils.parse_line
        |> Seq.fold_left
        (fun (idx, zeros) delta ->
            let zeros' = (abs(delta) / 100) + zeros in
            let idx' = ((idx + delta) mod 100 + 100) mod 100 in
            (* checking updated indeces for wrap around *)
            let crossing = if delta > 0 then idx' < idx
                else idx <> 0 && (idx' > idx || idx' == 0)
            in
            (idx', zeros' + if crossing then 1 else 0))
        (50, 0)
    in
    Printf.printf "Number of zeros: %d\n" zeros
