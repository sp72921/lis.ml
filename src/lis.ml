type 'a number

type atom =
  | Symbol of string
  | Number of int
  | Floating of float

type 'a lst = 'a list
type 'a expr = atom * 'a lst
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
  List.map
    (fun s ->
      let int' = int_of_string_opt s in
      match int' with
        None ->
         (match float_of_string_opt s with
          | None -> Symbol s
          | Some v -> Floating v)
      | Some v -> Number v) token

let rec read_from_tokens tokens =
  match tokens with
    [] -> failwith "Unexpected EOF"
  (* | [x] -> TODO Incestigate to simplify this sphagetti code furthur *)
  | h :: t -> if h = "(" then
                read_from_tokens t
              else if h = ")" then
                failwith "Syntax Error: Unxpected )"
              else atom t

let parse p =
  read_from_tokens (tokenize p)

let () =
  let program = "(begin (define r 10) (* pi (* r r)))" in
  parse program
