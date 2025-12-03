let run_day day_num part =
  match day_num with
  | 1 -> Day01.run part 
  | 2 -> Day02.run part
  | 3 -> Day03.run part
  | _ -> Printf.eprintf "Error: Day %d not found\n" day_num

let () =
  match Sys.argv with
  | [| _; day_str |] ->
      (try
         let day_num = int_of_string day_str in
         run_day day_num None
       with Failure _ ->
           Printf.eprintf "Usage: %s <day_number> [part]\n" Sys.argv.(0);
         exit 1)
  | [| _; day_str ; part_str |] ->
      (try
          let day_num = int_of_string day_str in
          let part_num = int_of_string part_str in
          run_day day_num (Some part_num)
        with Failure _ ->
            Printf.eprintf "Usage %s <day_numver [part]\n" Sys.argv.(0);
            exit 1)
  | _ ->
        Printf.eprintf "Usage: %s <day_number> [part]\n" Sys.argv.(0);
        exit 1
