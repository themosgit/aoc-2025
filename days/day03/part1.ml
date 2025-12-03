let file = "days/day03/input.txt"

let return_line input =
    Seq.of_dispenser (fun () -> In_channel.input_line input)

let find_max line =
    let (max1, max2) =
        let rec loop i max1 max2 =
            if i >= String.length line then (max1, max2)
            else
                let digit = Char.code line.[i] - Char.code '0' in
                if i < String.length line - 1 && digit > max1 then
                    loop (i + 1) digit (-1)
                else if digit > max2 then
                    loop (i + 1) max1 digit
                else 
                    loop (i + 1) max1 max2
        in
        loop 0 (-1) (-1)
    in
    max1 * 10 + max2


let solve () =
    In_channel.with_open_text file @@ fun input ->
    let sum =
        return_line input
        |> Seq.map find_max
        |> Seq.fold_left (+) 0
    in
    Printf.printf "Sum of jolts: %d\n" sum

