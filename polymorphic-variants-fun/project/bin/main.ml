let process_value = function
  | `Int n -> Printf.printf "Int: %d\n" n
  | `Float f -> Printf.printf "Float: %f\n" f
  | `String s -> Printf.printf "String: %s\n" s
  | `Bool b -> Printf.printf "Bool: %b\n" b

let print_shape = function
  | `Circle r -> Printf.printf "Circle with radius %f\n" r
  | `Rectangle (w, h) -> Printf.printf "Rectangle %f x %f\n" w h
  | `Point (x, y) -> Printf.printf "Point at (%f, %f)\n" x y

let combine (a : [< `Int of int | `Float of float]) (b : [< `Int of int | `Float of float]) =
  match a, b with
  | `Int x, `Int y -> `Int (x + y)
  | `Float x, `Float y -> `Float (x +. y)
  | `Int x, `Float y -> `Float (float_of_int x +. y)
  | `Float x, `Int y -> `Float (x +. float_of_int y)

let () =
  process_value (`Int 42);
  process_value (`String "hello");
  print_shape (`Circle 5.0);
  print_shape (`Rectangle (3.0, 4.0));
  match combine (`Int 10) (`Float 3.5) with
  | `Float f -> Printf.printf "Combined: %f\n" f
  | `Int i -> Printf.printf "Combined: %d\n" i
