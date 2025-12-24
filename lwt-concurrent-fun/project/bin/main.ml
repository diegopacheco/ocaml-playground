open Lwt.Syntax

let task1 () =
  let* () = Lwt_unix.sleep 0.1 in
  Lwt_io.printf "Task 1 completed\n"

let task2 () =
  let* () = Lwt_unix.sleep 0.2 in
  Lwt_io.printf "Task 2 completed\n"

let task3 () =
  let* () = Lwt_unix.sleep 0.05 in
  Lwt_io.printf "Task 3 completed\n"

let compute x =
  let* () = Lwt_unix.sleep 0.1 in
  Lwt.return (x * 2)

let () =
  Lwt_main.run begin
    let* () = Lwt.join [task1 (); task2 (); task3 ()] in
    let* results = Lwt.all [compute 5; compute 10; compute 15] in
    let* () = Lwt_io.printf "Results: " in
    Lwt_list.iter_s (fun x -> Lwt_io.printf "%d " x) results
  end
