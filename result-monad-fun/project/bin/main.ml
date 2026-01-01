let (>>=) r f =
  match r with
  | Ok v -> f v
  | Error e -> Error e

let return x = Ok x

let safe_div a b =
  if b = 0 then Error "Division by zero"
  else Ok (a / b)

let safe_sqrt x =
  if x < 0 then Error "Negative sqrt"
  else Ok (int_of_float (sqrt (float_of_int x)))

let compute x y z =
  safe_div x y >>= fun r1 ->
  safe_div r1 z >>= fun r2 ->
  safe_sqrt r2 >>= fun r3 ->
  return (r3 * 2)

let show_result = function
  | Ok v -> Printf.printf "Success: %d\n" v
  | Error e -> Printf.printf "Error: %s\n" e

let () =
  show_result (compute 100 5 2);
  show_result (compute 100 0 2);
  show_result (compute 100 5 0);
  show_result (safe_sqrt 16 >>= fun x -> safe_sqrt x)
