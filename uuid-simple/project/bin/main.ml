let uuid = (Uuidm.to_string (Uuidm.v4_gen(Random.State.make_self_init())()));;

let () = print_endline uuid
