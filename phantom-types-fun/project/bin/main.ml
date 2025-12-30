type locked
type unlocked

type 'a door = {
  name: string;
  code: int;
}

let make_door name code : unlocked door = {name; code}

let lock (d : unlocked door) : locked door =
  Printf.printf "Locking door: %s\n" d.name;
  {name = d.name; code = d.code}

let unlock code (d : locked door) : unlocked door option =
  if d.code = code then begin
    Printf.printf "Unlocking door: %s\n" d.name;
    Some {name = d.name; code = d.code}
  end else begin
    Printf.printf "Wrong code for door: %s\n" d.name;
    None
  end

let open_door (d : unlocked door) =
  Printf.printf "Opening door: %s\n" d.name

let () =
  let d = make_door "main entrance" 1234 in
  let locked_d = lock d in
  match unlock 1234 locked_d with
  | Some unlocked_d -> open_door unlocked_d
  | None -> Printf.printf "Failed to unlock\n"
