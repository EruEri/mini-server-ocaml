type http_method = 
| CONNECT
| DELETE
| GET
| HEAD
| OPTIONS
| POST
| PUT

type http_protocol = 
| HTTP_1_1;;

type http_response = {
  status_line : (http_protocol * int * string);
  headers : (string*string) list;
  body : bytes
}

type http_request = {
  status_line : (http_method * string * string);
  headers : (string*string) list;
  body : bytes
}

let string_of_day = function
| 0 -> "Sun"
| 1 -> "Mon"
| 2 -> "Tue"
| 3 -> "Wed"
| 4 -> "Thu"
| 5 -> "Fri"
| 6 -> "Sat"
| _ -> failwith "Wrong number for day"

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

let convert_year year = year + 1900

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


let string_of_http_method = function
| CONNECT -> "CONNECT"
| DELETE -> "DELETE"
| GET -> "GET"
| HEAD -> "HEAD"
| OPTIONS -> "OPTIONS"
| POST -> "POST"
| PUT -> "PUT"

let http_method_of_string s = match s with
| "CONNECT" -> CONNECT
| "DELETE" ->  DELETE
| "GET" ->  GET
| "HEAD" ->  HEAD
| "OPTIONS" -> OPTIONS
| "POST" ->  POST
| "PUT" -> PUT
| _ -> failwith "Doesn't match http method"



let string_of_http_protocol = function
| HTTP_1_1 -> "HTTP/1.1"

let ok = 200




let bytes_of_http_response (response: http_response) = 
  let (protocole, status, message) = response.status_line in
  let string_status_line = Printf.sprintf "%s %d %s\r\n" (protocole |> string_of_http_protocol) (status) (message) in
  let bytes_status = String.to_bytes string_status_line in
  let bytes_header = response.headers |> List.map (fun (header,value) -> Printf.sprintf "%s: %s" header value) |> String.concat "\r\n" |> String.to_bytes in
  bytes_status::bytes_header::("\r\n\r\n" |> String.to_bytes)::(response.body)::[] |> Bytes.concat Bytes.empty


let parse_request (request_bytes: bytes) = 
  let splited = request_bytes |> Bytes.to_string |> Str.split (Str.regexp "\r\n") in
  splited