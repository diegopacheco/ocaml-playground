class counter init = object
  val mutable count = init
  method get = count
  method inc = count <- count + 1
  method add n = count <- count + n
end

class ['a] stack = object
  val mutable items : 'a list = []
  method push x = items <- x :: items
  method pop = match items with
    | [] -> None
    | x :: xs -> items <- xs; Some x
  method size = List.length items
end

let () =
  let c = new counter 0 in
  c#inc;
  c#inc;
  c#add 5;
  Printf.printf "Counter: %d\n" c#get;

  let s = new stack in
  s#push 10;
  s#push 20;
  s#push 30;
  Printf.printf "Stack size: %d\n" s#size;
  match s#pop with
  | Some x -> Printf.printf "Popped: %d\n" x
  | None -> Printf.printf "Empty\n"
