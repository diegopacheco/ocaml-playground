type person = {
  name: string;
  age: int;
  scores: int list;
}

let serialize_to_bytes value =
  Marshal.to_bytes value []

let deserialize_from_bytes bytes =
  Marshal.from_bytes bytes 0

let () =
  let data = {name = "Alice"; age = 30; scores = [95; 87; 92]} in
  Printf.printf "Original: %s, age %d\n" data.name data.age;

  let bytes = serialize_to_bytes data in
  Printf.printf "Serialized to %d bytes\n" (Bytes.length bytes);

  let restored : person = deserialize_from_bytes bytes in
  Printf.printf "Restored: %s, age %d\n" restored.name restored.age;
  Printf.printf "Scores: %s\n" (String.concat ", " (List.map string_of_int restored.scores));

  let complex_data = ([1; 2; 3], "test", Some 42) in
  let bytes2 = serialize_to_bytes complex_data in
  let (lst, str, opt) = deserialize_from_bytes bytes2 in
  Printf.printf "List: %s, String: %s, Option: %d\n"
    (String.concat "," (List.map string_of_int lst)) str (Option.get opt)
