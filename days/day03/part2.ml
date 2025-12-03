let file = "days/day03/input.txt"

let return_line input =
    Seq.of_dispenser (fun () -> In_channel.input_line input)

let compute_start i line =
     let index = 12 - (String.length line - i) in
     if index < 0 then 0 else index

let rec set_joltarray j joltarray digit =
    if j >= 12 then joltarray
    else
        if joltarray.(j) < digit then begin
            joltarray.(j) <- digit;
            set_joltarray (j + 1) joltarray (-1)
        end
        else if digit == -1 then begin
            joltarray.(j) <- digit;
            set_joltarray (j + 1) joltarray digit
        end
        else
        set_joltarray (j + 1) joltarray digit



let sum_jolts line =
    let joltarray = Array.make 12 (-1) in
    let rec loop i =
        if i >= String.length line then ()
        else
            let digit = Char.code line.[i] - Char.code '0' in
            let _  = set_joltarray (compute_start i line) joltarray digit in
            loop (i + 1)
    in
    loop 0;
    let rec compute_sum i multiplier acc =
        if i < 0 then acc
        else compute_sum (i - 1) (multiplier * 10) 
            (acc + joltarray.(i) * multiplier)
    in
    compute_sum 11 1 0
    


let solve () =
    In_channel.with_open_text file @@ fun input ->
    let sum = 
        return_line input
        |> Seq.map sum_jolts 
        |> Seq.fold_left (+) 0
    in
    Printf.printf "Sum of jolts: %d\n" sum
