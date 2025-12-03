let file = "days/day03/input.txt"

(* change to 2 for part 1 *)
let jolt_size = 12 

(* retrieves new lines *)
let return_line input =
    Seq.of_dispenser (fun () -> In_channel.input_line input)

(** 
 * calculate earliest array idx that may be modified
 * if we have 10 chars left and jolt_size is 12 this
 * will return 2 meaning we can start modifying from
 * the second element in the array
 **)
let array_idx i line =
     let index = jolt_size - (String.length line - i) in
     if index < 0 then 0 else index

(**
 * checks if digit is max of some array member if
 * so resets all following members to -1 
 **)
let rec set_array idx joltarray digit =
    if idx >= jolt_size then joltarray
    else
        if joltarray.(idx) < digit then begin
            joltarray.(idx) <- digit;
            set_array (idx + 1) joltarray (-1)
        end
        else if digit = -1 then begin
            joltarray.(idx) <- -1;
            set_array (idx + 1) joltarray (-1) 
        end
        else
        set_array (idx + 1) joltarray digit

(* populates array with max digits and returns the sum *)
let compute_line line =
    let array = Array.make jolt_size (-1) in
    let rec populate_array i array =
        if i >= String.length line then array 
        else
            let digit = Char.code line.[i] - Char.code '0' in
            populate_array (i + 1) (set_array (array_idx i line) array digit)
    in
    let joltarray = populate_array 0 array in
    let rec sum_elements i mult accu =
        if i < 0 then accu
        else sum_elements (i - 1) (mult * 10) (accu + joltarray.(i) * mult)
    in
    sum_elements (jolt_size - 1) 1 0
    
let solve () =
    In_channel.with_open_text file @@ fun input ->
    let sum = 
        return_line input
        |> Seq.map compute_line 
        |> Seq.fold_left (+) 0
    in
    Printf.printf "Sum of jolts: %d\n" sum
