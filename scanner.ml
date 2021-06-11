exception Unknown_symbol of char

type t = {
  src: string;
  mutable ch: char;
  mutable offset: int;
}

let eof = Char.unsafe_chr (-1)

let init source =
  {
    src = source;
    ch = String.get source 0;
    offset = 0;
  }

let next scanner =
  let next_offset = scanner.offset + 1 in
  if next_offset < String.length scanner.src then (
    scanner.offset <- next_offset;
    scanner.ch <- String.get scanner.src next_offset;
  ) else (
    scanner.offset <- String.length scanner.src;
    scanner.ch <- eof;
  )

let peek scanner =
  if scanner.offset + 1 < String.length scanner.src then
    String.unsafe_get scanner.src (scanner.offset + 1)
  else
    eof

let get_scanned scanner start_offset =
  String.sub scanner.src start_offset (scanner.offset - start_offset)

let rec scan_digits scanner =
  match scanner.ch with
  | '0'..'9' -> next scanner; scan_digits scanner;
  | _ -> ()
let scan_number scanner =
  let start_offset = scanner.offset in
  scan_digits scanner;

  if scanner.ch != '/' then
    Token.Number ((get_scanned scanner start_offset), "1") 
  else (
    let numerator = (get_scanned scanner start_offset) in
    next scanner;
    let start_offset = scanner.offset in
    scan_digits scanner;
    Token.Number (numerator, (get_scanned scanner start_offset));
  )

let is_whitespace ch =
  match ch with
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let scan_identifier scanner =
  let start_offset = scanner.offset in
  let rec loop () =
    match scanner.ch with
    | '[' | ']' | '(' | ')' | ' ' | '\t' | '\n' | '\r' | '"' | '\'' ->
      ()
    | _ ->
      next scanner;
      loop ()
  in
    loop ();
    Token.Identifier (get_scanned scanner start_offset)

let rec skip_whitespace scanner =
  if is_whitespace scanner.ch then (
    next scanner;
    skip_whitespace scanner;
  ) else ()

let scan_string scanner =
  let start_offset = scanner.offset in
  let rec loop () =
    match scanner.ch with
    | '"' ->
      let token = Token.String (get_scanned scanner start_offset) in
        next scanner;
        token
    | _ -> next scanner; loop ()
  in
    loop ()

let scan scanner =
  skip_whitespace scanner;
  let token = match scanner.ch with
  | '(' ->
    next scanner;
    Token.LParen
  | ')' ->
    next scanner;
    Token.RParen
  | '0'..'9' -> scan_number scanner
  | '\'' ->
    next scanner;
    Token.Quote
  | '"' ->
    next scanner;
    scan_string scanner;
  | ch when ch == eof -> Token.Eof
  | _ ->
    (* anything could possibly be a identifier *)

    scan_identifier scanner;
  in
    token