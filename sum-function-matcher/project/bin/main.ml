let rec sum l = 
  match l with 
   [] -> 0. 
  | h :: t -> h +. (sum t);;

let result = (sum [1.;2.;3.;4.;5.;6.;]);;

let () = print_endline (String.concat " " ["\n" ; "Sum 1 + 2 + 3 + 4 +5 + 6 ==" ; string_of_float result]);;
