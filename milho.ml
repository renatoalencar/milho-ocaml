
let rec print_all_tokens scanner =
  let token = Scanner.scan scanner in
  token |> Token.to_string |> print_endline;
  match token with
  | Token.Eof -> ()
  | _ -> print_all_tokens scanner

let make_ident size =
  String.make size ' '

let rec string_of_expression ident exp =
  make_ident ident ^
  match exp with
  | Reader.List xs ->
    let interal_serialized_expression =
      xs
      |> List.map (string_of_expression (ident + 2))
      |> String.concat "\n"
    in
      "(\n" ^ interal_serialized_expression ^ (make_ident ident) ^ ")\n"
  | Reader.Number (n, d) ->
    Printf.sprintf "%d/%d" n d
  | Reader.String s ->
    "\"" ^ s ^ "\""
  | Reader.False ->
    "False"
  | Reader.True ->
    "True"
  | Reader.Nil ->
    "Nil"
  | Reader.Symbol s ->
    "'" ^ s
  | Reader.Quote e ->
    "'" ^ string_of_expression ident e
  | Reader.Eof ->
    "\n"

let read_file channel =
  let size = in_channel_length channel in
  let buf = Bytes.create size in
  really_input channel buf 0 size;
  Bytes.unsafe_to_string buf

let run_file scope name =
  let rec loop_in_expressions scanner scope =
    let exp = Reader.read scanner in
    try
      exp
      |> Runtime.stop_on_eof
      |> Runtime.value_of_expression
      |> Runtime.eval scope
      |> ignore;

      loop_in_expressions scanner scope
    with
    | Runtime.Stop_execution -> ()
  in
  let source = name |> open_in |> read_file in
  let scanner = Scanner.init source in
    loop_in_expressions scanner scope

let () =
  if (Array.length Sys.argv) < 2 then
    print_endline "Need a file, example ./milho example.milho"
  else
    let scope = Scope.init () in
      run_file scope "core.milho";
      run_file scope Sys.argv.(1)