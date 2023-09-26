let i = 1;;

let result = match i with
| 0 -> "zero"
| 1 -> "one"
| _ -> "some other number";;

let () = print_endline (String.concat "\n" [""; result]);;