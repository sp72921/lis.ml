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
  match int_of_string_opt token with
    None ->
     (match float_of_string_opt token with
      | None -> Symbol token
      | Some v -> Floating v)
  | Some v -> Number v

let rec read_from_tokens tokens =
  match tokens with
  | [] -> []
  | "(" :: t ->
       let l = Util.find_opt "(" t in
       let r = Util.find_opt ")" t in
       let l_opt = Option.is_some l in
       if l_opt then
         (if l < r then
            Util.take (Option.get l) t :: read_from_tokens t
          else (Util.take (Option.get l - 1) t) :: read_from_tokens t)
       else Util.take (Option.get r) t :: read_from_tokens t
  | _ :: t -> read_from_tokens t

let parse p =
  read_from_tokens (tokenize p) |> List.map (List.map atom)

type paren =
  { str: (char * char);
    match_it: (string * string);
    mutable count: int;
    is_balanced: (int -> bool);
    (* mutable pos: int *)
  }

let check_parens l =
  let p =
    {str = ('(', ')');
     match_it = ("(", ")");
     count = 0;
     is_balanced = (fun n -> n = 0)} in
  match
    String.starts_with ~prefix:(fst p.match_it) l
    && String.ends_with ~suffix:(snd p.match_it) l
  with
  | true ->
     String.iter
       (fun a ->
         if Char.equal a (fst p.str) then
           p.count <- p.count + 1
         else if Char.equal a (snd p.str) then
           p.count <- p.count - 1) l;
     (p.is_balanced (p.count))
  | false -> false

let print_atom_lists l =
  List.iter
    (List.iter (fun a ->
         match a with
         | Symbol s -> Printf.printf "Symbol: %s\n" s
         | Number n -> Printf.printf "Number: %d\n" n
         | Floating f -> Printf.printf "Floating: %f\n" f)) l

