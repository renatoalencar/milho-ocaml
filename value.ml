type t =
| List of t list
| Symbol of string
| Number of int * int
| String of string
| Function of (t list -> t)
| Macro of (t list -> t)
| True
| False
| Nil
| Quote of t

let rec to_string v =
  match v with
  | List items ->
    "'(" ^ (items |> List.map to_string |> String.concat " ") ^ ")"
  | Symbol name -> "'" ^ name
  | Number (n, d) -> Printf.sprintf "%d/%d" n d
  | String s -> s
  | Function _ -> "Function"
  | Macro _ -> "Macro"
  | False -> "False"
  | True -> "True"
  | Nil -> "Nil"
  | Quote v -> "'" ^ (to_string v)

let rec to_boolean v =
  match v with
  | List [] -> false
  | Number (0, _) -> false
  | String "" -> false
  | False -> false
  | Nil -> false
  | Quote v -> to_boolean v
  | _ -> true