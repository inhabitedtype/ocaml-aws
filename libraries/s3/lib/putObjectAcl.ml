open Types
open Aws
type input = PutObjectAclRequest.t
type output = PutObjectAclOutput.t
type error = Errors_internal.t
let service = "s3"
let to_http req =
  let uri =
    Uri.add_query_params (Uri.of_string "https://s3.amazonaws.com")
      (List.append
         [("Version", ["2006-03-01"]); ("Action", ["PutObjectAcl"])]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (PutObjectAclRequest.to_query req))))) in
  (`PUT, uri, [])
let of_http body =
  try
    let xml = Ezxmlm.from_string body in
    let resp = Xml.member "PutObjectAclResponse" (snd xml) in
    try
      Util.or_error (Util.option_bind resp PutObjectAclOutput.parse)
        (let open Error in
           BadResponse
             {
               body;
               message = "Could not find well formed PutObjectAclOutput."
             })
    with
    | Xml.RequiredFieldMissing msg ->
        let open Error in
          `Error
            (BadResponse
               {
                 body;
                 message =
                   ("Error parsing PutObjectAclOutput - missing field in body or children: "
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