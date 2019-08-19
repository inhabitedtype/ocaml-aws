open Types
open Aws
type input = LookupEventsRequest.t
type output = LookupEventsResponse.t
type error = Errors_internal.t
let service = "cloudtrail"
let to_http req =
  let uri =
    Uri.add_query_params (Uri.of_string "https://cloudtrail.amazonaws.com")
      (List.append
         [("Version", ["2013-11-01"]); ("Action", ["LookupEvents"])]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (LookupEventsRequest.to_query req))))) in
  (`POST, uri, [])
let of_http body =
  try
    let xml = Ezxmlm.from_string body in
    let resp = Xml.member "LookupEventsResponse" (snd xml) in
    try
      Util.or_error (Util.option_bind resp LookupEventsResponse.parse)
        (let open Error in
           BadResponse
             {
               body;
               message = "Could not find well formed LookupEventsResponse."
             })
    with
    | Xml.RequiredFieldMissing msg ->
        let open Error in
          `Error
            (BadResponse
               {
                 body;
                 message =
                   ("Error parsing LookupEventsResponse - missing field in body or children: "
                      ^ msg)
               })
  with
  | Failure msg ->
      `Error
        (let open Error in
           BadResponse { body; message = ("Error parsing xml: " ^ msg) })
let parse_error code err =
  let errors =
    [Errors_internal.InvalidNextToken;
    Errors_internal.InvalidMaxResults;
    Errors_internal.InvalidTimeRange;
    Errors_internal.InvalidLookupAttributes] @ Errors_internal.common in
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