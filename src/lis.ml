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
       let l = find_opt "(" t in
       let r = find_opt ")" t in
       let l_opt = Option.is_some l in
       if l_opt then (
         if l < r then
           take (Option.get l) t :: read_from_tokens t
         else (take (Option.get l - 1) t) :: read_from_tokens t
       )
       else take (Option.get r) t :: read_from_tokens t
     else read_from_tokens t

let parse p =
  read_from_tokens (tokenize p) |> List.map (List.map atom)

type paren =
  { str: (char * char);
    mutable count: int}

let check_parens l =
  let paren' = {str = ('(', ')'); count = 0} in
  String.iter
    (fun a ->
      if Char.equal a (fst paren'.str) then
        (paren'.count <- paren'.count + 1;
         Printf.printf "%d " paren'.count)
      else if Char.equal a (snd paren'.str) then
        (paren'.count <- paren'.count - 1;
         Printf.printf "%d " paren'.count)
    ) l

let print_atom_lists l =
  List.iter
    (List.iter (fun a ->
         match a with
         | Symbol s -> Printf.printf "Symbol: %s\n" s
         | Number n -> Printf.printf "Number: %d\n" n
         | Floating f -> Printf.printf "Floating: %f\n" f)) l

let () =
  let program = "(begin (define r 10) (* pi (* r r)))" in
  parse program |> print_atom_lists
