let () =
  let open Parcoom in
  let _input = "hello = [1, 2, 3]" in
  let _parser = tag "hello" <|> tag "falopa" in
  failwith "TODO"
;;

let hello_parser = Parcoom.tag "hello"
let whitespace_parser = Parcoom.many0 (Parcoom.character ' ')

let _spaced_hello_parser input =
  let open Parcoom in
  (whitespace_parser *> hello_parser <* whitespace_parser) input
;;
