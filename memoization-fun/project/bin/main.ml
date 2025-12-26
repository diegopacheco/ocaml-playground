let memoize f =
  let cache = Hashtbl.create 128 in
  fun x ->
    match Hashtbl.find_opt cache x with
    | Some y -> Printf.printf "Cache hit for %d\n" x; y
    | None ->
        Printf.printf "Computing for %d\n" x;
        let y = f x in
        Hashtbl.add cache x y;
        y

let expensive_compute n =
  n * n * n

let square = memoize (fun x -> x * x)
let cube = memoize expensive_compute

let () =
  Printf.printf "Square 5: %d\n" (square 5);
  Printf.printf "Square 5 again: %d\n" (square 5);
  Printf.printf "Cube 3: %d\n" (cube 3);
  Printf.printf "Cube 3 again: %d\n" (cube 3);
  Printf.printf "Cube 4: %d\n" (cube 4);
  Printf.printf "Square 5 third time: %d\n" (square 5)
