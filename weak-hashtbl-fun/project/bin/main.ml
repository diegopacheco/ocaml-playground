module WeakCache = Weak.Make(struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end)

let create_cache () =
  let cache = WeakCache.create 10 in
  let add_string s =
    WeakCache.add cache s;
    Printf.printf "Added: %s\n" s
  in
  let check_string s =
    match WeakCache.find_opt cache s with
    | Some str -> Printf.printf "Found in cache: %s\n" str; true
    | None -> Printf.printf "Not in cache: %s\n" s; false
  in
  (add_string, check_string)

let () =
  let (add, check) = create_cache () in
  add "hello";
  add "world";
  add "ocaml";
  ignore (check "hello");
  ignore (check "world");
  ignore (check "notfound");
  Gc.full_major ();
  Printf.printf "After GC:\n";
  ignore (check "hello")
