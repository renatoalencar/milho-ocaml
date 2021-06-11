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