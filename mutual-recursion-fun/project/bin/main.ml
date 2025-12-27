let rec is_even n =
  if n = 0 then true
  else if n < 0 then is_even (-n)
  else is_odd (n - 1)

and is_odd n =
  if n = 0 then false
  else if n < 0 then is_odd (-n)
  else is_even (n - 1)

let rec eval_expr = function
  | `Int n -> n
  | `Add (e1, e2) -> eval_expr e1 + eval_expr e2
  | `Mul (e1, e2) -> eval_expr e1 * eval_expr e2
  | `Var name -> lookup_var name

and lookup_var = function
  | "x" -> 10
  | "y" -> 20
  | _ -> 0

let () =
  Printf.printf "is_even 4: %b\n" (is_even 4);
  Printf.printf "is_odd 4: %b\n" (is_odd 4);
  Printf.printf "is_even 7: %b\n" (is_even 7);
  Printf.printf "is_odd 7: %b\n" (is_odd 7);
  Printf.printf "eval: %d\n" (eval_expr (`Add (`Var "x", `Mul (`Int 2, `Var "y"))))
