exception Unexpected_right_paren

type expression =
  | List of expression list
  | Number of int * int
  | String of string
  | Symbol of string
  | Quote of expression
  | True
  | False
  | Nil
  | Eof

let rec read scanner =
  let token = Scanner.scan scanner in
  match token with
  | Token.LParen ->
    read_list scanner
  | Token.RParen ->
    raise Unexpected_right_paren
  | Token.String s ->
    String s
  | Token.Number (n, d) ->
    Number ((int_of_string n), (int_of_string d))
  | Token.Identifier id -> (
    match id with
    | "True" -> True
    | "False" -> False
    | "Nil" -> Nil
    | name -> Symbol name
  )
  | Token.Eof ->
    Eof
  | Token.Quote ->
    Quote (read scanner)
and read_list scanner =
  let rec loop exp =
    try
      loop (read scanner :: exp)
    with
    | Unexpected_right_paren ->
      exp
  in
    List (List.rev (loop []))
