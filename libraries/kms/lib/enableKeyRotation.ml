open Types
open Aws

type input = EnableKeyRotationRequest.t

type output = unit

type error = Errors_internal.t

let service = "kms"

let signature_version = Request.V4

let to_http service region req =
  let uri =
    Uri.add_query_params
      (Uri.of_string (Aws.Util.of_option_exn (Endpoints.url_of service region)))
      (List.append
         [ "Version", [ "2014-11-01" ]; "Action", [ "EnableKeyRotation" ] ]
         (Util.drop_empty
            (Uri.query_of_encoded (Query.render (EnableKeyRotationRequest.to_query req)))))
  in
  `POST, uri, []

let of_http body = `Ok ()

let parse_error code err =
  let errors = [] @ Errors_internal.common in
  match Errors_internal.of_string err with
  | Some var ->
      if List.mem var errors
         &&
         match Errors_internal.to_http_code var with
         | Some var -> var = code
         | None -> true
      then Some var
      else None
  | None -> None
