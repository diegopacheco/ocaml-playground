let from_list lst = ([], lst)

let go_right (left, right) =
  match right with
  | [] -> None
  | x :: xs -> Some (x :: left, xs)

let go_left (left, right) =
  match left with
  | [] -> None
  | x :: xs -> Some (xs, x :: right)

let update f (left, right) =
  match right with
  | [] -> (left, right)
  | x :: xs -> (left, f x :: xs)

let to_list (left, right) =
  List.rev_append left right

let rec move_right n z =
  if n <= 0 then Some z
  else match go_right z with
  | None -> None
  | Some z' -> move_right (n - 1) z'

let () =
  let z = from_list [1; 2; 3; 4; 5] in
  let z = Option.get (move_right 2 z) in
  let z = update (fun x -> x * 10) z in
  let z = Option.get (go_left z) in
  Printf.printf "Result: %s\n" (String.concat " " (List.map string_of_int (to_list z)))
