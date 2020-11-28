open Types
open Aws
type input = HeadObjectRequest.t
type output = HeadObjectOutput.t
type error = Errors_internal.t
let service = "s3"
let to_http service region (req : input) =
  let uri =
    Uri.add_query_params
      (Uri.of_string @@
         ((^) req.bucket
            (Aws.Util.of_option_exn (Endpoints.url_of service region))))
      (List.append [("Version", ["2006-03-01"]); ("Action", ["HeadObject"])]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (HeadObjectRequest.to_query req))))) in
  (`HEAD, uri, [])
let of_http body =
  try
    let xml = Ezxmlm.from_string body in
    let resp = Xml.member "HeadObjectResponse" (snd xml) in
    try
      Util.or_error (Util.option_bind resp HeadObjectOutput.parse)
        (let open Error in
           BadResponse
             { body; message = "Could not find well formed HeadObjectOutput."
             })
    with
    | Xml.RequiredFieldMissing msg ->
        let open Error in
          `Error
            (BadResponse
               {
                 body;
                 message =
                   ("Error parsing HeadObjectOutput - missing field in body or children: "
                      ^ msg)
               })
  with
  | Failure msg ->
      `Error
        (let open Error in
           BadResponse { body; message = ("Error parsing xml: " ^ msg) })
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