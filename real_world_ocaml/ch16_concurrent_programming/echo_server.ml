(* write an echo server, i.e., a program that accepts connections from clients and spits back whatever is sent to it.  *)

open Core
open Async

(* a function that can copy data from an input to an output *)
(* Async’s Reader and Writer modules, which provide a convenient abstraction for working with input and output channels *)
let rec copy_blocks buffer r w =
  match%bind Reader.read r buffer with
  | `Eof -> return ()
  | `Ok bytes_read ->
      Writer.write w (Bytes.to_string buffer) ~len:bytes_read;
      let%bind () = Writer.flushed w in
      copy_blocks buffer r w

(** Starts a TCP server, which listens on the specified port, invoking copy_blocks every time a
    client connects. *)
let server_run () =
  let host_and_port =
    Tcp.Server.create ~on_handler_error:`Raise (Tcp.Where_to_listen.of_port 8080) (fun _addr r w ->
        let buffer = Bytes.create (16 * 1024) in
        copy_blocks buffer r w)
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

let server_run_v2 port =
  let buffer_size = 16 * 1024 in
  let server_port = Tcp.Where_to_listen.of_port port in

  let handle_client addr reader writer =
    printf "New connection from %s\n" (Socket.Address.Inet.to_string addr);
    let buffer = Bytes.create buffer_size in
    let%bind () = copy_blocks buffer reader writer in
    printf "Connection closed from %s\n" (Socket.Address.Inet.to_string addr);
    Deferred.unit
  in
  let%bind _server = Tcp.Server.create ~on_handler_error:`Raise server_port handle_client in
  printf "Echo server started on port %d\n" port;
  Deferred.never ()

let server_run_v3 ~uppercase ~port =
  let server_port = Tcp.Where_to_listen.of_port port in

  let handle_client addr reader writer =
    printf "New connection from %s\n" (Socket.Address.Inet.to_string addr);
    let%bind () =
      Pipe.transfer (Reader.pipe reader) (Writer.pipe writer)
        ~f:(if uppercase then String.uppercase else Fn.id)
    in
    printf "Connection closed from %s\n" (Socket.Address.Inet.to_string addr);
    Deferred.unit
  in
  let%bind _server = Tcp.Server.create ~on_handler_error:`Raise server_port handle_client in
  printf "Echo server started on port %d\n" port;
  Deferred.never ()

(* To start server: 
dune exec real_world_ocaml/ch16_concurrent_programming/ch16.exe demo03 -- -p 3000 *)
(* On another terminal: echo "This is an echo server" | nc 127.0.0.1 3000 *)
let command =
  (* 1. create Command.Param *)
  let params =
    let%map_open.Command port = flag "-p" (required int) ~doc:"port The port to listen on"
    and uppercase =
      flag "-u"
        (optional_with_default false bool)
        ~doc:"Convert to uppercase (default is lowercase)"
    in
    fun () -> server_run_v3 ~uppercase ~port
  in
  (* 2. create readme *)
  let readme () =
    "This command demonstrates an echo server in OCaml. It listens on a specified port and echoes \
     back any data sent to it. You can specify the port using the -p flag."
  in
  Command.async ~summary:"Demo echo server" ~readme params
