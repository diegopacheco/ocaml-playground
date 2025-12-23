type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | Eq : int expr * int expr -> bool expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr

let rec eval : type a. a expr -> a = function
  | Int n -> n
  | Bool b -> b
  | Add (e1, e2) -> eval e1 + eval e2
  | Eq (e1, e2) -> eval e1 = eval e2
  | If (cond, e1, e2) -> if eval cond then eval e1 else eval e2

let () =
  let e1 = Add (Int 5, Int 3) in
  let e2 = Eq (Int 10, Add (Int 5, Int 5)) in
  let e3 = If (Bool true, Int 42, Int 0) in
  let e4 = If (Eq (Int 5, Int 5), Add (Int 10, Int 20), Int 0) in
  Printf.printf "%d\n" (eval e1);
  Printf.printf "%b\n" (eval e2);
  Printf.printf "%d\n" (eval e3);
  Printf.printf "%d\n" (eval e4)
