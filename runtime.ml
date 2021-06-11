exception Stop_execution
exception Runtime_error of string

let stop_on_eof exp =
  match exp with
  | Reader.Eof -> raise Stop_execution
  | e -> e

let rec value_of_expression exp =
  match exp with
  | Reader.List items -> Value.List (List.map value_of_expression items)
  | Reader.Number (n, d) -> Value.Number (n, d)
  | Reader.String s -> Value.String s
  | Reader.False -> Value.False
  | Reader.True -> Value.True
  | Reader.Nil -> Value.Nil
  | Reader.Symbol name -> Value.Symbol name
  | Reader.Quote expression -> value_of_expression expression
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
  | value -> value
and let_binding scope args =
  let bindings = List.hd args in
  let body = List.tl args in
  let rec eval_list scope forms =
    match forms with
    | [] -> Value.Nil
    | [form] -> eval scope form
    | form :: rest ->
        eval scope form |> ignore;
        eval_list scope rest
  in
    eval_list (scope_from_bindings scope bindings) body
and scope_from_bindings scope bindings =
  match bindings with
  | Value.List (name :: value :: _) -> (
    match name with
    | Value.Symbol name -> Scope.push scope name (eval scope value)
    | form -> raise (Runtime_error ("Invalid name " ^ (Value.to_string form)))
  )
  | form -> raise (Runtime_error ("Invalid binding " ^ (Value.to_string form)))
and def_binding scope args =
  match args with
  | name :: value :: _ -> (
    match name with
    | Value.Symbol name ->
      let value = eval scope value in
        Scope.def scope name value;
        value
    | form -> raise (Runtime_error ("Invalid name " ^ (Value.to_string form)))
  )
  | _ -> raise (Runtime_error "Invalid binding")