let file = "days/day04/input.txt"

let return_line input =
    Seq.of_dispenser (fun () -> In_channel.input_line input)

let count_adjacent grid row col =
    let rows = Array.length grid in
    let cols = String.length grid.(0) in
    let rec check_dir dirs acc =
        match dirs with
        | [] -> acc
        | (dr, dc) :: rest ->
            let nr = row + dr in
            let nc = col + dc in
            if nr >= 0 && nr < rows &&
               nc >= 0 && nc < cols &&
               grid.(nr).[nc] = '@'
            then check_dir rest (acc + 1)
            else check_dir rest acc
    in
    check_dir [(-1,-1); (-1,0); (-1,1); (0,-1);
               (0,1); (1,-1); (1,0); (1,1)] 0

let count_accessible grid row =
    let cols = String.length grid.(row) in
    let rec loop col acc =
        if col >= cols then acc
        else if grid.(row).[col] = '@' && count_adjacent grid row col < 4
        then loop (col + 1) (acc + 1)
        else loop (col + 1) acc
    in
    loop 0 0

let solve () =
    In_channel.with_open_text file @@ fun input ->
    let grid = return_line input 
    |> Array.of_seq in
    Seq.init (Array.length grid) (fun i -> i)
    |> Seq.map (count_accessible grid)
    |> Seq.fold_left (+) 0
    |> Printf.printf "Reachable rolls: %d\n"
