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

let rec eval scope exp =
  match exp with
  | Value.List xs -> (
    match xs with
    | [] -> Value.Nil
    | f :: args -> (
      match f with
      | Value.Symbol "let" -> let_binding scope args
      | Value.Symbol "def" -> def_binding scope args
      | Value.Symbol "fn" -> anon_fn scope args
      | Value.Symbol "if" -> if_form scope args
      | f -> (
        match eval scope f with
        | Value.Function f' -> f' (List.map (eval scope) args)
        | Value.Macro f' -> f' args
        | value ->
          raise (Runtime_error ((Value.to_string value) ^ " is not a function"))
      )
    )
  )
  | Value.Symbol s ->
    Scope.find scope s
  | Value.Quote v -> v
  | value -> value
and eval_list scope forms =
  List.fold_left (fun _ form -> eval scope form) Value.Nil forms
and bind scope name value =
  Scope.def scope (expect_symbol "Invalid name " name) value;
  scope
and let_binding scope args =
  match args with
  | bindings :: body ->
    eval_list (scope_from_bindings scope bindings) body
  | _ -> raise (Runtime_error "Invalid let binding ")   
and scope_from_bindings scope bindings =
  bindings
  |> expect_list "Invalid binding "
  |> fold_pair_left bind (Scope.push_empty scope)
and def_binding scope args =
  match args with
  | name :: value :: _ -> (
    let value = eval scope value in
      Scope.def scope (expect_symbol "Invalid name " name) value;
      value
  )
  | _ -> raise (Runtime_error "Invalid binding")
and anon_fn scope args_and_body =
  let bind_arguments args =
    args_and_body
    |> List.hd
    |> (expect_list "Not a list")
    |> List.fold_left2 (fun s v n -> bind s n v) (Scope.push_empty scope) args
  in
  let fn args =
    args_and_body
    |> List.tl
    |> eval_list (bind_arguments args)
  in
    Value.Function fn
and if_form scope forms =
  match forms with
  | predicate :: consequense :: [] ->
    if predicate |> eval scope |> Value.to_boolean then
      eval scope consequense
    else
      Value.Nil
  | predicate :: consequense :: otherwise :: [] ->
    if predicate |> eval scope |> Value.to_boolean then
      eval scope consequense
    else
      eval scope otherwise
  | _ -> Value.Nil