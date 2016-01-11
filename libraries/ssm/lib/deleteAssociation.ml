open Types
open Aws
type input = DeleteAssociationRequest.t
type output = unit
type error = Errors.t
let service = "ssm"
let to_http req =
  let uri =
    Uri.add_query_params (Uri.of_string "https://ssm.amazonaws.com")
      (List.append
         [("Version", ["2014-11-06"]); ("Action", ["DeleteAssociation"])]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (DeleteAssociationRequest.to_query req))))) in
  (`POST, uri, [])
let of_http body = `Ok ()
let parse_error code err =
  let errors =
    [Errors.TooManyUpdates;
    Errors.InvalidInstanceId;
    Errors.InvalidDocument;
    Errors.InternalServerError;
    Errors.AssociationDoesNotExist] @ Errors.common in
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