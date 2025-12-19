type error = ..

type error += ParseError of string
type error += NetworkError of int * string
type error += FileNotFound of string

let describe_error = function
  | ParseError msg -> Printf.sprintf "Parse error: %s" msg
  | NetworkError (code, msg) -> Printf.sprintf "Network error %d: %s" code msg
  | FileNotFound path -> Printf.sprintf "File not found: %s" path
  | _ -> "Unknown error"

type error += DatabaseError of string

let handle_error err =
  match err with
  | DatabaseError msg -> Printf.printf "DB: %s\n" msg
  | e -> Printf.printf "%s\n" (describe_error e)

let () =
  handle_error (ParseError "invalid JSON");
  handle_error (NetworkError (404, "not found"));
  handle_error (FileNotFound "/tmp/data.txt");
  handle_error (DatabaseError "connection timeout");

  let errors = [ParseError "syntax"; NetworkError (500, "server"); DatabaseError "locked"] in
  List.iter handle_error errors
