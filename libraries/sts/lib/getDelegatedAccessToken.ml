open Aws

type input = GetDelegatedAccessTokenRequest.t
type output = GetDelegatedAccessTokenResponse.t
type error = Errors_internal.t

let service = "sts"
let signature_version = Request.V4

let to_http service region req =
  let uri =
    Uri.add_query_params
      (Uri.of_string (Aws.Util.of_option_exn (Endpoints.url_of service region)))
      (List.append
         [ "Version", [ "2011-06-15" ]; "Action", [ "GetDelegatedAccessToken" ] ]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (GetDelegatedAccessTokenRequest.to_query req)))))
  in
  `POST, uri, [], ""

let of_http body =
  try
    let xml = Ezxmlm.from_string body in
    let resp =
      Util.option_bind
        (Xml.member "GetDelegatedAccessTokenResponse" (snd xml))
        (Xml.member "GetDelegatedAccessTokenResult")
    in
    try
      Util.or_error
        (Util.option_bind resp GetDelegatedAccessTokenResponse.parse)
        (let open Error in
         BadResponse
           { body
           ; message = "Could not find well formed GetDelegatedAccessTokenResponse."
           })
    with Xml.RequiredFieldMissing msg ->
      let open Error in
      `Error
        (BadResponse
           { body
           ; message =
               "Error parsing GetDelegatedAccessTokenResponse - missing field in body or \
                children: "
               ^ msg
           })
  with Failure msg ->
    `Error
      (let open Error in
       BadResponse { body; message = "Error parsing xml: " ^ msg })

let parse_error code err =
  let errors = [] @ Errors_internal.common in
  match Errors_internal.of_string err with
  | Some var ->
      if
        List.mem var errors
        &&
        match Errors_internal.to_http_code var with
        | Some var -> var = code
        | None -> true
      then Some var
      else None
  | None -> None
