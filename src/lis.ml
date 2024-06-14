type atom =
  | Symbol of string
  | Number of int
  | Floating of float

type 'a env = ('a * 'a) list

let tokenize (chars: string) : string list =
  String.fold_left
  (fun acc c ->
    match c with
      '(' -> acc ^ " ( "
    | ')' -> acc ^ " ) "
    | _ -> acc ^ String.make 1 c
  )
  "" chars
  |> String.split_on_char ' '
  |> List.filter (fun c -> c <> "")

let atom token =
  let int' = int_of_string_opt token in
  match int' with
    None ->
     (match float_of_string_opt token with
      | None -> Symbol token
      | Some v -> Floating v)
  | Some v -> Number v

let rec find_opt x lst =
    match lst with
    | [] -> None
    | h :: t -> if x = h
                then Some 0 else
                  match find_opt x t with
                  | None -> None
                  | Some idx -> Some (idx + 1)

let rec take n lst =
  if n = 0 then [] else
    match lst with
    | [] -> []
    | h :: t -> h :: take (n - 1) t

let rec drop n l =
  if n = 0 then l
  else match l with
       | [] -> []
       | h :: t -> drop (n - 1) t

let rec read_from_tokens tokens =
  match tokens with
  | [] -> []
  | h :: t ->
     if String.equal h "(" then
       (match find_opt "(" t with
        | None ->
           (match find_opt ")" t with
            | None -> failwith "UNMATCHED `)'"
            | Some v -> (take v t) :: read_from_tokens t
           )
        | Some n -> (take n t) :: read_from_tokens t)
     else read_from_tokens t

let parse p =
  read_from_tokens (tokenize p) |> List.map (List.map atom)

let _ =
  let program = "(begin (define r 10) (* pi (* r r)))" in
  parse program
