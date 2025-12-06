let file = "days/day06/input.txt"

let return_line input =
    Seq.of_dispenser (fun () -> In_channel.input_line input)

let parse_array func line =
    String.split_on_char ' ' line
    |> List.filter (fun s -> String.length s > 0)
    |> List.map func
    |> Array.of_list

let parse_numbers = parse_array int_of_string
let parse_ops = parse_array Fun.id

let apply_op op a b c d =
    match op with
    | "+" -> a + b + c + d
    | "*" -> a * b * c * d
    | _ -> failwith "unknown op"

let solve () =
    In_channel.with_open_text file @@ fun input ->
    let grid = return_line input
        |> Seq.take 4
        |> Seq.map parse_numbers
        |> Array.of_seq
    in
    let ops = In_channel.input_line input
        |> Option.get
        |> parse_ops
    in

    let cols = Array.length grid.(0) in
    let rec calculate col acc =
        if col >= cols then acc
        else
            let result = apply_op ops.(col)
            grid.(0).(col) grid.(1).(col) grid.(2).(col) grid.(3).(col) in
            calculate (col + 1) (acc + result)
    in
    let total = calculate 0 0 in
    Printf.printf "Total: %d\n" total
