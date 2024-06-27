open Lis

let run p =
  if check_parens p
  then parse p
       |> print_atom_lists
  else Printf.printf "ERROR: Unmatched parentheses\n"

let _ =
  try
    while true do
      Printf.printf "user> ";
      flush stdout;
      let program = read_line () in
      run program
    done;
  with
    End_of_file -> failwith "read_line failed"
