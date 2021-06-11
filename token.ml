type t =
| LParen
| RParen
| Identifier of string
| Number of string * string
| String of string
| Quote
| Eof


let to_string = function
| LParen -> "("
| RParen -> ")"
| Number (n, d) -> ("Number " ^ n ^ "/" ^ d)
| Identifier id -> ("ID " ^ id)
| String str -> ("String \"" ^ str ^ "\"")
| Quote -> "Quote"
| Eof -> "EOF"