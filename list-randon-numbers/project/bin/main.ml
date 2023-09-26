open Printf

let result = (List.init 16 (fun x -> Random.int 1000+x));;

let () = List.iter (printf "%d ") result;;
