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

let remove_accessible grid row =
    let cols = String.length grid.(row) in
    let rec loop col acc =
        if col >= cols then acc
        else
            let ch = grid.(row).[col] in
            let new_ch =
                if ch = '@' && count_adjacent grid row col < 4 then '.'
                else ch
            in
            loop (col + 1) (acc ^ String.make 1 new_ch)
    in
    loop 0 ""

let process_until_stable grid =
    let rec loop current iteration =
        let new_grid =
            Seq.init (Array.length current) (fun i -> i)
            |> Seq.map (remove_accessible current)
            |> Array.of_seq
        in
        let grids_equal =
            let rec check_rows i =
                if i >= Array.length current then true
                else if String.equal current.(i) new_grid.(i) then check_rows (i + 1)
                else false
            in
            check_rows 0
        in
        if grids_equal then
            (current, iteration)
        else loop new_grid (iteration + 1)
    in
    loop grid 0

let count_symbols grid =
    Seq.init (Array.length grid) (fun i -> grid.(i))
    |> Seq.map (fun line ->
        let rec count_col col sum =
            if col >= String.length line then sum
            else if line.[col] = '@' then count_col (col + 1) (sum + 1)
            else count_col (col + 1) sum
        in
        count_col 0 0
    )
    |> Seq.fold_left (+) 0

let solve () =
    In_channel.with_open_text file @@ fun input ->
    let grid = return_line input
    |> Array.of_seq in
    let initial_count = count_symbols grid in
    let final_grid, iterations = process_until_stable grid in
    let removed = initial_count - count_symbols final_grid in
    Printf.printf "Removed %d rolls after %d iterations\n" removed iterations
