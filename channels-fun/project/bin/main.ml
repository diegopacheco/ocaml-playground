let channel = Event.new_channel ()

let sender ch values =
  List.iter (fun v ->
    Printf.printf "Sending: %d\n" v;
    Event.sync (Event.send ch v);
    Thread.delay 0.1
  ) values

let receiver ch count =
  for i = 1 to count do
    let v = Event.sync (Event.receive ch) in
    Printf.printf "Received: %d\n" v
  done

let () =
  let values = [1; 2; 3; 4; 5] in
  let t1 = Thread.create (fun () -> sender channel values) () in
  let t2 = Thread.create (fun () -> receiver channel (List.length values)) () in
  Thread.join t1;
  Thread.join t2;
  Printf.printf "All messages processed\n"
