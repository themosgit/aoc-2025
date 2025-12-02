let file = "days/day02/input.txt"

let is_invalid id =
    let s = string_of_int id in
    let len = String.length s in
    let rec check substrlen =
        if substrlen > len / 2 then false
        else
            if len mod substrlen == 0 && String.sub s 0 (len - substrlen) =
                String.sub s substrlen (len - substrlen) then true
            else check (substrlen + 1)
        in
        check 1

let return_invalid_ids (num1, num2) =
    let rec loop curr () =
        if curr > num2 then Seq.Nil
        else if is_invalid curr then
            Seq.Cons (curr, loop (curr + 1))
        else
            loop (curr + 1) ()
        in
        loop num1

let solve () =
    In_channel.with_open_text file @@ fun input ->
    let total =
        Utils.parse_range input
        |> Seq.flat_map return_invalid_ids
        |> Seq.fold_left(+) (0)
    in 
    Printf.printf "Sum of invalid ID's: %d\n" total
