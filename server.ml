(* unix.cma http.cmo server.ml *)

open Unix;;
open Sys;;
open Http;;

let host = gethostbyname (gethostname());;

let local_ip = 
  try 
  let host = host in
  Some host.h_addr_list.(0)
  with Not_found -> None
  | Invalid_argument (s) -> prerr_endline s; None
;;

external read_response: file_descr -> unit -> bytes = "caml_read_response"

let default_response socket () =
  let file = open_in_bin "index.html" in
  let file_bytes = really_input_string file (file |> in_channel_length) |> String.to_bytes in
  let protocol = HTTP_1_1 in
  let status_line = protocol, Http.Response.ok, "OK" in
  let date = "Date", Http.current_date_http_format in
  let content_length = "Content-Length", (file_bytes |> Bytes.length |> string_of_int)  in
  let content_type = "Content-Type", "text/html" in
  let (response: Http. Response.http_response) = Http.Response.create status_line [date; content_length; content_type] file_bytes  in
  let bytes_reposne = Http.Response.to_bytes response in
  let read = Unix.send socket (bytes_reposne) 0 ((bytes_reposne |> Bytes.length)) ([]) in
  read

let string_of_sockadrr = function
| Unix.ADDR_INET(inet, port) -> Printf.sprintf "%s:%d" (Unix.string_of_inet_addr inet) port
| Unix.ADDR_UNIX name -> name
;;
let run () = 
  let socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  let server_sockadrr =  (ADDR_INET (local_ip |> Option.get , 8080) ) in
  Unix.bind socket server_sockadrr;
  Unix.listen socket 1;

  let _ = Sys.signal sigint (Sys.Signal_handle (fun signal -> close socket; print_endline "\nServer stop listening"; exit 0) ) in
  print_endline (Printf.sprintf "Ocaml Start listening at %s ..."  (string_of_sockadrr server_sockadrr));
  let rec listening () = 
        let file_client, sockadress = Unix.accept socket in
        print_endline ("New connection from "^(sockadress |> string_of_sockadrr));
        match Http.Request.http_request_of_request_bytes_result (read_response file_client ()) with
        | Error e -> ()
        | Ok http_request -> print_endline (Http.Request.string_of_http_request http_request);
        let bytes_send = default_response file_client () in
        
        if bytes_send == -1 then print_endline "Error response";
        listening () in
  listening ()
;;

run ()


(* let string_of_host_entry (host: host_entry) =
  Printf.sprintf "%s" (host.h_addr_list |> Array.map Unix.string_of_inet_addr |> Array.to_list |> String.concat "\n");;

print_endline (string_of_host_entry host) *)

