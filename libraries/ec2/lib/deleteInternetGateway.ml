open Types_internal
open Aws
type input = DeleteInternetGatewayRequest.t
type output = unit
type error = Errors_internal.t
let service = "ec2" 
let to_http req =
  let uri =
    Uri.add_query_params (Uri.of_string "https://ec2.amazonaws.com")
      (List.append
         [("Version", ["2015-04-15"]); ("Action", ["DeleteInternetGateway"])]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (DeleteInternetGatewayRequest.to_query req)))))
     in
  (`POST, uri, []) 
let of_http body = `Ok () 
let parse_error code err =
  let errors = [] @ Errors_internal.common  in
  match Errors_internal.of_string err with
  | Some var ->
      if
        (List.mem var errors) &&
          ((match Errors_internal.to_http_code var with
            | Some var -> var = code
            | None  -> true))
      then Some var
      else None
  | None  -> None 