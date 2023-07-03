open Base

type 'a parser = string -> (string * 'a, string) Result.t

let character (c : char) : char parser =
 fun input ->
  let input_len = String.length input in
  let expected_but_got expected got =
    Printf.sprintf "Parcoom: expected `%s` but got `%s`" expected got
  in
  if input_len < 1
  then Error (expected_but_got (Char.escaped c) input)
  else (
    let extracted = input.[0] in
    if Char.equal c extracted
    then Ok (String.sub input ~pos:1 ~len:(input_len - 1), extracted)
    else Error (expected_but_got (Char.escaped c) (Char.escaped extracted)))
;;

let tag (tag : string) : string parser =
 fun input ->
  let tag_len = String.length tag in
  let input_len = String.length input in
  let expected_but_got expected got =
    Printf.sprintf "Parcoom: expected `%s` but got `%s`" expected got
  in
  if tag_len > input_len
  then Error (expected_but_got tag input)
  else (
    match String.sub input ~pos:0 ~len:tag_len with
    | extracted when String.equal extracted tag ->
      let rest = String.sub input ~pos:tag_len ~len:(input_len - tag_len) in
      Ok (rest, tag)
    | extracted -> Error (expected_but_got tag extracted))
;;

let ( <|> ) (a : 'a parser) (b : 'a parser) : 'a parser =
 fun input ->
  match a input with
  | Ok res -> Ok res
  | Error a_error ->
    (match b input with
     | Ok res -> Ok res
     | Error b_error -> Error (a_error ^ " | " ^ b_error))
;;

let ( *> ) (a : 'a parser) (b : 'b parser) : 'b parser =
 fun input -> a input |> Result.map ~f:(fun (a_rest, _) -> b a_rest) |> Result.join
;;

let ( <* ) (a : 'a parser) (b : 'b parser) : 'a parser =
 fun input ->
  a input
  |> Result.map ~f:(fun (a_rest, a_val) ->
       b a_rest |> Result.map ~f:(fun (b_rest, _) -> b_rest, a_val))
  |> Result.join
;;

let ( <*> ) (a : 'a parser) (b : 'b parser) : ('a * 'b) parser =
 fun input ->
  match a input with
  | Error a_err -> Error a_err
  | Ok (a_rest, a_val) ->
    b a_rest |> Result.map ~f:(fun (b_rest, b_val) -> b_rest, (a_val, b_val))
;;

let run (input : string) (a : 'a parser) = a input

(* Many0 and Many1 might need to reverse the result array to have good results *)
let many0 (p : 'a parser) : 'a list parser =
 fun input ->
  let rec loop input' result =
    match p input' with
    | Ok (rest, v) -> loop rest (v :: result)
    | Error _ -> Ok (input', result)
  in
  loop input []
;;

let many1 (p : 'a parser) : 'a list parser =
 fun input ->
  let rec loop input' result =
    match p input' with
    | Ok (rest, v) -> loop rest (v :: result)
    | Error e ->
      (match result with
       | [] ->
         Error (Printf.sprintf "Parcoom: many1 expected to parse at least one: (%s)" e)
       | _ -> Ok (input', result))
  in
  loop input []
;;

let separated_by ~separator:(s : 's parser) (p : 'p parser) : 'p list parser =
 fun input ->
  let rec loop input result =
    match p input with
    | Ok (rest, v) -> loop rest (v :: result)
    | Error _ ->
      (match s input with
       | Ok (rest, _) -> loop rest result
       | Error _ -> Ok (input, result))
  in
  loop input []
;;
