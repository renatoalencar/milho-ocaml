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

let def scope name value =
  let current = List.hd scope in
    Hashtbl.add current name value

let push scope name value =
  let tbl = Hashtbl.create 1024 in
    Hashtbl.add tbl name value;
    tbl :: scope

let push_empty scope =
  let tbl = Hashtbl.create 1024 in
    tbl :: scope

let rec find scope name =
  match scope with
  | [] -> raise (Variable_not_found (name ^ " variable is not defined"))
  | current :: upper ->
    if Hashtbl.mem current name then 
      Hashtbl.find current name
    else
      find upper name
