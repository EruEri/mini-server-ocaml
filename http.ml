type http_protocol = 
| HTTP_1_1;;

let string_of_http_protocol = function
| HTTP_1_1 -> "HTTP/1.1"
;;
let http_protocol_of_string_opt = function
| "HTTP/1.1" -> Some HTTP_1_1
| _ -> None
;;


let string_of_day = function
| 0 -> "Sun"
| 1 -> "Mon"
| 2 -> "Tue"
| 3 -> "Wed"
| 4 -> "Thu"
| 5 -> "Fri"
| 6 -> "Sat"
| _ -> failwith "Wrong number for day"
;;

let string_of_month = function
| 0 -> "Jan"
| 1 -> "Feb"
| 2 -> "Mar"
| 3 -> "Apr"
| 4 -> "May"
| 5 -> "Jun"
| 6 -> "Jul"
| 7 -> "Aug"
| 8 -> "Sep"
| 9 -> "Oct"
| 10 -> "Nov"
| 11 -> "Dec"
| _ -> failwith "Wrong number for month"
;;


let convert_year = ( + ) 1900
;;

let current_date_http_format = 
  let tm = Unix.time () |> Unix.localtime in
  Printf.sprintf "%s, %02d %s %d %02d:%02d:%02d GMT" 
  (string_of_day tm.tm_wday)
  (tm.tm_mday)
  (string_of_month tm.tm_mon)
  (convert_year tm.tm_year)
  (tm.tm_hour)
  (tm.tm_min)
  (tm.tm_sec)
;;



module Request : sig
  type http_request
  type http_method
  type parse_error

  val string_of_http_method: http_method -> string
  val http_request_of_request_bytes_result: bytes -> (http_request, parse_error) result
  val http_method_of_string: string -> http_method
  val http_method_of_string_opt: string -> http_method option
  val string_of_http_request: http_request -> string
end
= struct

  type http_method = 
    | CONNECT
    | DELETE
    | GET
    | HEAD
    | OPTIONS
    | POST
    | PUT
  ;;

  type http_request = {
    status_line : (http_method * string * http_protocol);
    headers : (string*string) list;
    body : bytes
  }

  type parse_error =
    | Request_Wrong_Format_Error
    | Status_Line_Missing_Error
    | Status_line_Wrong_Format_Error
    | Unknown_Http_Method_Error
    | Unsupported_Portocol_Error
    | Double_CRLF_Split_Error
    | No_Content
  ;;
  let crlf_regex = Str.regexp "\r\n";;
  let double_crlf_regex = Str.regexp "\r\n\r\n";;

  let http_method_of_string = function
    | "CONNECT" -> CONNECT
    | "DELETE" ->  DELETE
    | "GET" ->  GET
    | "HEAD" ->  HEAD
    | "OPTIONS" -> OPTIONS
    | "POST" ->  POST
    | "PUT" -> PUT
    | _ -> failwith "Doesn't match http method"
  ;;

  let http_method_of_string_opt = function
    | "CONNECT" -> Some CONNECT
    | "DELETE" ->  Some DELETE
    | "GET" ->  Some GET
    | "HEAD" ->  Some HEAD
    | "OPTIONS" -> Some OPTIONS
    | "POST" ->  Some POST
    | "PUT" -> Some PUT
    | _ -> None
  ;;

  let string_of_http_method = function
    | CONNECT -> "CONNECT"
    | DELETE -> "DELETE"
    | GET -> "GET"
    | HEAD -> "HEAD"
    | OPTIONS -> "OPTIONS"
    | POST -> "POST"
    | PUT -> "PUT"
  ;;

  let string_of_http_request request = 
    let { status_line; headers; body } = request in
    let ( meth, route, protocol ) = status_line in
    Printf.sprintf "%s %s %s\n%s\n%s" 
    (string_of_http_method meth)
    (route)
    (string_of_http_protocol protocol)
    (headers |> List.map (fun (header, value) -> Printf.sprintf "%s: %s" (header) (value) ) |> String.concat "\n")
    (body |> Bytes.to_string)
  ;;
  
  let status_line_string_of_request_bytes (request_bytes: bytes) = 
    request_bytes |> Bytes.to_string |> Str.split crlf_regex |> List.hd
  ;;
  let status_line_string_of_request_bytes_opt (request_bytes: bytes) =
    match request_bytes |> Bytes.to_string |> Str.split crlf_regex with
    | [] -> None
    | t::_ -> Some t
  ;;

  let status_line_of_string_result status_line_str = 
    match status_line_str |> String.split_on_char ' ' with
    | meth::route::protocol::[] -> (
      match (http_method_of_string_opt meth, http_protocol_of_string_opt protocol) with
      | Some h_method, Some h_protocol -> Ok (h_method, route, h_protocol)
      | None, Some _ | None, None -> Error Unknown_Http_Method_Error
      | Some _, None -> Error Unsupported_Portocol_Error
    )
    | _ -> Error Status_line_Wrong_Format_Error
  ;;

  let headers_of_request_bytes_result (request_bytes: bytes) =
    match request_bytes |> Bytes.to_string |> Str.split double_crlf_regex with
    | request_info::_ -> begin
      request_info 
      |> Str.split crlf_regex 
      |> List.filter_map (fun s -> match s |> String.split_on_char ':' with | header::value::[] -> Some (header, value) | _  -> None)
      |> Result.ok
    end
    | _ -> Error Double_CRLF_Split_Error
  ;;

  let content_bytes_request_bytes_result (request_bytes: bytes) = 
    match request_bytes |> Bytes.to_string |> Str.split double_crlf_regex with
    | _::bytes::[] -> bytes |> String.to_bytes |> Result.ok
    | _ -> Error No_Content
  ;;

  let http_request_of_request_bytes_result (request_bytes: bytes) = 
    match status_line_string_of_request_bytes_opt request_bytes with
    | None -> Error Request_Wrong_Format_Error
    | Some status_string -> begin
      match status_line_of_string_result status_string with
      | Error e -> Error e
      | Ok status_line -> (
        match (headers_of_request_bytes_result request_bytes, content_bytes_request_bytes_result request_bytes) with
        | Ok headers, Ok content -> Ok { status_line; headers; body = content }
        | Ok headers, Error _ -> Ok { status_line; headers; body = Bytes.empty}
        | Error e1, Ok _ | Error e1, Error _ -> Error e1
      )
    end
  ;;
end



module Response : sig
  type http_response

  val ok: int
  val error404: int

  val create: (http_protocol * int * string) -> (string*string) list -> bytes -> http_response
  val to_bytes: http_response -> bytes
end
= struct
  type http_response = {
    status_line : (http_protocol * int * string);
    headers : (string*string) list;
    body : bytes
  }
  let ok = 200;;
  let error404 = 404;;

  let create status_line headers body = {
    status_line;
    headers;
    body
  }

  let to_bytes (response: http_response) = 
    let (protocol, status, message) = response.status_line in
    let string_status_line = Printf.sprintf "%s %d %s\r\n" (protocol |> string_of_http_protocol) (status) (message) in
    let bytes_status = String.to_bytes string_status_line in
    let bytes_header = response.headers |> List.map (fun (header,value) -> Printf.sprintf "%s: %s" header value) |> String.concat "\r\n" |> String.to_bytes in
    bytes_status::bytes_header::("\r\n\r\n" |> String.to_bytes)::(response.body)::[] |> Bytes.concat Bytes.empty
  ;;
end





