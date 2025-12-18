type _ Effect.t += Log : string -> unit Effect.t
type _ Effect.t += Ask : int Effect.t

let logger () =
  Effect.perform (Log "Starting computation");
  let x = Effect.perform Ask in
  Effect.perform (Log "Got value from ask");
  Printf.printf "Computation result: %d\n" (x * 2);
  x * 2

let run_logger f =
  Effect.Deep.try_with f ()
  { effc = fun (type a) (eff : a Effect.t) ->
      match eff with
      | Log msg -> Some (fun (k : (a, _) Effect.Deep.continuation) ->
          Printf.printf "[LOG] %s\n" msg;
          Effect.Deep.continue k ())
      | Ask -> Some (fun (k : (a, _) Effect.Deep.continuation) ->
          Printf.printf "[ASK] Returning 21\n";
          Effect.Deep.continue k 21)
      | _ -> None
  }

let () =
  Printf.printf "Running effect handler:\n";
  let result = run_logger logger in
  Printf.printf "Final result: %d\n" result
