module type Serializer = sig
  type t
  val serialize : t -> string
  val deserialize : string -> t option
end

module IntSerializer = struct
  type t = int
  let serialize x = string_of_int x
  let deserialize s = try Some (int_of_string s) with _ -> None
end

module StringSerializer = struct
  type t = string
  let serialize x = x
  let deserialize s = Some s
end

let show_serialize (type a) (module S : Serializer with type t = a) (value : a) =
  Printf.printf "Serialized: %s\n" (S.serialize value)

let roundtrip (type a) (module S : Serializer with type t = a) (value : a) =
  let serialized = S.serialize value in
  match S.deserialize serialized with
  | Some _ -> Printf.printf "Roundtrip successful\n"
  | None -> Printf.printf "Roundtrip failed\n"

let () =
  let serializers = [
    (module IntSerializer : Serializer with type t = int);
  ] in
  show_serialize (module IntSerializer) 42;
  show_serialize (module StringSerializer) "hello world";
  roundtrip (module IntSerializer) 99;
  List.iter (fun (module S : Serializer with type t = int) ->
    Printf.printf "Using module: %s\n" (S.serialize 100)
  ) serializers
