open Types
open Aws
type input = DeleteTrailRequest.t
type output = unit
type error = Errors.t
let service = "cloudtrail" 
let to_http req =
  let uri =
    Uri.add_query_params (Uri.of_string "https://cloudtrail.amazonaws.com")
      (List.append [("Version", ["2013-11-01"]); ("Action", ["DeleteTrail"])]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (DeleteTrailRequest.to_query req)))))
     in
  (`POST, uri, []) 
let of_http body = `Ok () 
let parse_error code err =
  let errors =
    [Errors.InvalidTrailName; Errors.TrailNotFound] @ Errors.common  in
  match Errors.of_string err with
  | Some var ->
      if
        (List.mem var errors) &&
          ((match Errors.to_http_code var with
            | Some var -> var = code
            | None  -> true))
      then Some var
      else None
  | None  -> None 