exception Invalid_argument of string

let sum args =
  let sum a b =
    match a, b with
    | Value.Number (an, ad), Value.Number (bn, bd) ->
      Value.Number ((an * bd) + (bn *ad), ad * bd)
    | _ -> Value.Nil
  in
    List.fold_left sum (Number (0, 1)) args

let sub args =
  let negative x =
    match x with
    | Value.Number (a, b) -> Value.Number (-a, b)
    | _ -> Value.Nil
  in
  let sub a b =
    match a with
    | Value.Nil -> b
    | Value.Number _ -> sum [a; (negative b)]
    | _ -> Value.Nil
  in
    List.fold_left sub Value.Nil args
  
let mul args =
  let mul a b =
    match a, b with
    | Value.Number (an, ad), Value.Number (bn, bd) ->
      Value.Number (an * bn, ad * bd)
    | _ -> Value.Nil
  in
    List.fold_left mul (Number (1, 1)) args

let div args =
  let inverse x =
    match x with
    | Value.Number (a, b) -> Value.Number (b, a)
    | _ -> Value.Nil
  in
  let div a b =
    match a with
    | Value.Nil -> b
    | Value.Number _ -> mul [a; (inverse b)]
    | _ -> Value.Nil
  in
    List.fold_left div Value.Nil args

let abs args =
  match args with
  | Value.Number (n, d) :: [] ->
    Value.Number (Stdlib.abs n, Stdlib.abs d)
  | _ -> raise (Invalid_argument "Milho_core.abs")

let cmp args =
  match args with
  | a :: b :: _ ->
    let a = abs [a] in
    let b = abs [b] in
      sub [a; b]
  | _ -> Value.Nil

let less_than args =
  match cmp args with
  | Value.Number (n, _) ->
    if n < 0 then
      Value.True
    else
      Value.False
  | _ -> raise (Invalid_argument "Milho_core.less_than")

let greater_than args =
  match cmp args with
  | Value.Number (n, _) ->
    if n > 0 then
      Value.True
    else
      Value.False
  | _ -> raise (Invalid_argument "Milho_core.less_than")

let equal args =
  match args with
  | a :: b :: _ -> (
    match a, b with
    | Value.Symbol a, Value.Symbol b ->
      Value.of_boolean (a == b)
    | Value.String a, Value.String b ->
      Value.of_boolean (a == b)
    | Value.True, Value.True ->
      Value.True
    | Value.False, Value.False ->
      Value.True
    | Value.Nil, Value.Nil ->
      Value.True
    | Value.Number _, Value.Number _ ->
      Value.of_boolean (Value.to_number a == Value.to_number b)
    | _ ->
      Value.False
  )
  | _ -> raise (Invalid_argument "Milho_core.equal")

let str args =
  args
  |> List.map Value.to_string
  |> String.concat " "

let println args =
  args
  |> str
  |> print_endline;

  Value.Nil

let rec cons args =
  match args with
  | [] -> Value.List []
  | value :: [] -> Value.List [value]
  | value :: lst :: [] -> (
    match lst with
    | Value.List lst -> Value.List (value :: lst)
    | v -> Value.List [value; v]
  )
  | value :: rest ->
    cons [value; (cons rest)]

let car args =
  match args with
  | Value.List (value :: _) :: [] -> value
  | _ -> raise (Invalid_argument "Milho_core.car")

let cdr args =
  match args with
  | Value.List (_ :: tl ) :: [] -> Value.List tl
  | _ -> raise (Invalid_argument "Milho_core.cdr")