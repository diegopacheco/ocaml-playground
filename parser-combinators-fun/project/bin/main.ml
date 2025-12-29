type 'a parser = string -> int -> ('a * int) option

let return x _ pos = Some (x, pos)

let bind p f input pos =
  match p input pos with
  | None -> None
  | Some (v, pos') -> f v input pos'

let (>>=) = bind

let char c input pos =
  if pos < String.length input && input.[pos] = c then
    Some (c, pos + 1)
  else None

let (<|>) p1 p2 input pos =
  match p1 input pos with
  | Some _ as result -> result
  | None -> p2 input pos

let digit input pos =
  if pos < String.length input && input.[pos] >= '0' && input.[pos] <= '9' then
    Some (int_of_char input.[pos] - int_of_char '0', pos + 1)
  else None

let () =
  let add_parser : int parser = digit >>= fun d1 -> char '+' >>= fun _ -> digit >>= fun d2 -> return (d1 + d2) in
  let alt_parser = char 'x' <|> char 'y' in
  match add_parser "3+5" 0 with
  | Some (result, _) -> Printf.printf "Result: %d\n" result
  | None -> Printf.printf "Parse failed\n";
  match alt_parser "y" 0 with
  | Some (c, _) -> Printf.printf "Parsed char: %c\n" c
  | None -> Printf.printf "No match\n"
