exception Stop_execution
exception Runtime_error of string

let stop_on_eof exp =
  match exp with
  | Reader.Eof -> raise Stop_execution
  | e -> e

let rec fold_pair_left f acc l =
  match l with
  | [] -> acc
  | [_] -> raise (Failure "List has not an even size")
  | a :: b :: l -> fold_pair_left f (f acc a b) l

let expect_symbol message value =
  match value with
  | Value.Symbol name -> name
  | value -> raise (Runtime_error (message ^ (Value.to_string value)))

let expect_list message value =
  match value with
  | Value.List lst -> lst
  | value -> raise (Runtime_error (message ^ (Value.to_string value)))

let rec value_of_expression exp =
  match exp with
  | Reader.List items -> Value.List (List.map value_of_expression items)
  | Reader.Number (n, d) -> Value.Number (n, d)
  | Reader.String s -> Value.String s
  | Reader.False -> Value.False
  | Reader.True -> Value.True
  | Reader.Nil -> Value.Nil
  | Reader.Symbol name -> Value.Symbol name
  | Reader.Quote expression -> Value.Quote (value_of_expression expression)
  | Reader.Eof -> Value.Nil

let bind env name value =
  Environment.def env (expect_symbol "Invalid name " name) value;
  env

let rec eval env exp =
  match exp with
  | Value.List xs -> (
    match xs with
    | [] -> Value.Nil
    | f :: args -> (
      match f with
      | Value.Symbol "let" -> let_binding env args
      | Value.Symbol "def" -> def_binding env args
      | Value.Symbol "fn" -> anon_fn env args
      | Value.Symbol "if" -> if_form env args
      | Value.Symbol "macro" -> anon_macro env args
      | f -> (
        match eval env f with
        | Value.Function f' ->
          args
          |> List.map (eval env)
          |> f'
        | Value.Macro f' ->
          args
          |> f'
          |> eval env
        | value ->
          raise (Runtime_error ((Value.to_string value) ^ " is not a function"))
      )
    )
  )
  | Value.Symbol s ->
    Environment.find env s
  | Value.Quote v -> v
  | value -> value
and eval_list env forms =
  List.fold_left (fun _ form -> eval env form) Value.Nil forms
and let_binding env args =
  match args with
  | bindings :: body ->
    eval_list (env_from_bindings env bindings) body
  | _ -> raise (Runtime_error "Invalid let binding ")
and bind_eval env name value =
  value
  |> eval env
  |> bind env name
and env_from_bindings env bindings =
  bindings
  |> expect_list "Invalid binding "
  |> fold_pair_left bind_eval (Environment.push_empty env)
and def_binding env args =
  match args with
  | name :: value :: _ -> (
    let value = eval env value in
      Environment.def env (expect_symbol "Invalid name " name) value;
      value
  )
  | _ -> raise (Runtime_error "Invalid binding")
and create_fn env args_and_body =
  let bind_arguments args =
    args_and_body
    |> List.hd
    |> (expect_list "Not a list")
    |> List.fold_left2 (fun s v n -> bind s n v) (Environment.push_empty env) args
  in
  let fn args =
    args_and_body
    |> List.tl
    |> eval_list (bind_arguments args)
  in
    fn
and anon_fn env args_and_body =
  Value.Function (create_fn env args_and_body)
and anon_macro env args_and_body =
  Value.Macro (create_fn env args_and_body)
and if_form env forms =
  match forms with
  | predicate :: consequense :: [] ->
    if predicate |> eval env |> Value.to_boolean then
      eval env consequense
    else
      Value.Nil
  | predicate :: consequense :: otherwise :: [] ->
    if predicate |> eval env |> Value.to_boolean then
      eval env consequense
    else
      eval env otherwise
  | _ -> Value.Nil