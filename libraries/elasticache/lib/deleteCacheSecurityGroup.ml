open Types
open Aws
type input = DeleteCacheSecurityGroupMessage.t
type output = unit
type error = Errors.t
let service = "elasticache"
let to_http req =
  let uri =
    Uri.add_query_params (Uri.of_string "https://elasticache.amazonaws.com")
      (List.append
         [("Version", ["2015-02-02"]);
         ("Action", ["DeleteCacheSecurityGroup"])]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (DeleteCacheSecurityGroupMessage.to_query req))))) in
  (`POST, uri, [])
let of_http body = `Ok ()
let parse_error code err =
  let errors =
    [Errors.InvalidParameterCombination;
    Errors.InvalidParameterValue;
    Errors.CacheSecurityGroupNotFound;
    Errors.InvalidCacheSecurityGroupState] @ Errors.common in
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