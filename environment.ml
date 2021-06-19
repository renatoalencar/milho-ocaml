exception Variable_not_found of string

type t = (string, Value.t) Hashtbl.t list

let init () =
  let builtin =
    let tbl = Hashtbl.create 10 in
      Hashtbl.add tbl "+" (Value.Function Milho_core.sum);
      Hashtbl.add tbl "-" (Value.Function Milho_core.sub);
      Hashtbl.add tbl "*" (Value.Function Milho_core.mul);
      Hashtbl.add tbl "/" (Value.Function Milho_core.div);

      Hashtbl.add tbl "<" (Value.Function Milho_core.less_than);
      Hashtbl.add tbl ">" (Value.Function Milho_core.greater_than);
      Hashtbl.add tbl "=" (Value.Function Milho_core.equal);

      Hashtbl.add tbl "cons" (Value.Function Milho_core.cons);
      Hashtbl.add tbl "car" (Value.Function Milho_core.car);
      Hashtbl.add tbl "cdr" (Value.Function Milho_core.cdr);

      Hashtbl.add tbl "println" (Value.Function Milho_core.println);
      tbl
  in
    [builtin]

let def env name value =
  let current = List.hd env in
    Hashtbl.add current name value

let push env name value =
  let tbl = Hashtbl.create 1024 in
    Hashtbl.add tbl name value;
    tbl :: env

let push_empty env =
  let tbl = Hashtbl.create 1024 in
    tbl :: env

let rec find env name =
  match env with
  | [] -> raise (Variable_not_found (name ^ " variable is not defined"))
  | current :: upper ->
    if Hashtbl.mem current name then 
      Hashtbl.find current name
    else
      find upper name
