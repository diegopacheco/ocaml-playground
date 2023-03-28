let deriv f dx = fun x -> (f (x +. dx) -. f x) /. dx;;
let sin' = deriv sin 1e-6;;
let compose f g = fun x -> f (g x);;
let cos2 = compose sqrt cos;;
let result = List.map (fun n -> n * 2 + 1) [0;1;2;3;4];;

print_string (String.concat " " (List.map string_of_int result))

