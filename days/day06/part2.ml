let file = "days/day06/input.txt"

let return_line input =
    Seq.of_dispenser (fun () -> In_channel.input_line input)

let parse_col grid c =
    let rows = Array.length grid in
    let last_row = rows - 1 in
    let op = if c < String.length grid.(last_row) then grid.(last_row).[c] else ' ' in
    let num_str =
        Seq.init last_row (fun r -> 
            if c < String.length grid.(r) then grid.(r).[c] else ' ')
        |> Seq.filter (fun ch -> ch <> ' ')
        |> String.of_seq
    in
    let num = if num_str = "" then None else Some (int_of_string num_str) in
    (num, op)

let eval nums op =
    match op with
    | '+' -> List.fold_left (+) 0 nums
    | '*' -> List.fold_left ( * ) 1 nums
    | _ -> 0

let solve () =
    In_channel.with_open_text file @@ fun input ->
    let grid = return_line input
    |> Array.of_seq in
    let width = Array.fold_left (fun acc s ->
        max acc (String.length s)) 0 grid in

    let rec calculate c nums op total =
        if c >= width then total + eval nums op
        else
            match parse_col grid c with
            | None, ' ' -> calculate (c + 1) [] ' ' (total + eval nums op)
            | n_opt, c_op ->
                let next_op = if c_op <> ' ' then c_op else op in
                let next_nums = Option.fold ~none:nums ~some:(fun n -> n :: nums) n_opt in
                calculate (c + 1) next_nums next_op total
    in
    
    let total = calculate 0 [] ' ' 0 in
    Printf.printf "Total: %d\n" total
