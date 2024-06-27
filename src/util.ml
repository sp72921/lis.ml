let rec take n lst =
  if n = 0 then [] else
    match lst with
    | [] -> []
    | h :: t -> h :: take (n - 1) t

let rec drop n l =
  if n = 0 then l
  else match l with
       | [] -> []
       | _ :: t -> drop (n - 1) t

let rec slice i k l =
  take (k + 1) l |> drop i

let rec find_opt x lst =
    match lst with
    | [] -> None
    | h :: t -> if x = h
                then Some 0 else
                  match find_opt x t with
                  | None -> None
                  | Some idx -> Some (idx + 1)
