open Types
open Aws
type input = DeleteBucketLifecycleRequest.t
type output = unit
type error = Errors_internal.t
let service = "s3"
let signature_version = Request.S3
let to_http service region req =
  let uri =
    Uri.add_query_params
      (Uri.of_string
         (Aws.Util.of_option_exn (Endpoints.url_of service region)))
      (List.append
         [("Version", ["2006-03-01"]); ("Action", ["DeleteBucketLifecycle"])]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (DeleteBucketLifecycleRequest.to_query req))))) in
  (`DELETE, uri, [])
let of_http body = `Ok ()
let parse_error code err =
  let errors = [] @ Errors_internal.common in
  match Errors_internal.of_string err with
  | Some var ->
      if
        (List.mem var errors) &&
          ((match Errors_internal.to_http_code var with
            | Some var -> var = code
            | None -> true))
      then Some var
      else None
  | None -> None