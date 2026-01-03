let create_socket () =
  Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0

let bind_socket sock addr =
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock addr;
  Printf.printf "Socket bound\n"

let listen_socket sock backlog =
  Unix.listen sock backlog;
  Printf.printf "Socket listening with backlog %d\n" backlog

let () =
  try
    Printf.printf "Creating TCP server socket\n";
    let sock = create_socket () in
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 9999) in
    bind_socket sock addr;
    listen_socket sock 5;
    Printf.printf "Server ready on port 9999\n";
    Unix.close sock;
    Printf.printf "Socket closed\n"
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e)
