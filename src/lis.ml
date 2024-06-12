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

let () =
  let program = "(begin (define r 10) (* pi (* r r)))" in
  tokenize program
