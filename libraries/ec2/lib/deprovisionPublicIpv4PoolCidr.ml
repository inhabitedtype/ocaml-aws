open Aws

type input = DeprovisionPublicIpv4PoolCidrRequest.t
type output = DeprovisionPublicIpv4PoolCidrResult.t
type error = Errors_internal.t

let service = "ec2"
let signature_version = Request.V4

let to_http service region req =
  let uri =
    Uri.add_query_params
      (Uri.of_string (Aws.Util.of_option_exn (Endpoints.url_of service region)))
      (List.append
         [ "Version", [ "2016-11-15" ]; "Action", [ "DeprovisionPublicIpv4PoolCidr" ] ]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (DeprovisionPublicIpv4PoolCidrRequest.to_query req)))))
  in
  `POST, uri, [], ""

let of_http body =
  try
    let xml = Ezxmlm.from_string body in
    let resp = Xml.member "DeprovisionPublicIpv4PoolCidrResponse" (snd xml) in
    try
      Util.or_error
        (Util.option_bind resp DeprovisionPublicIpv4PoolCidrResult.parse)
        (let open Error in
         BadResponse
           { body
           ; message = "Could not find well formed DeprovisionPublicIpv4PoolCidrResult."
           })
    with Xml.RequiredFieldMissing msg ->
      let open Error in
      `Error
        (BadResponse
           { body
           ; message =
               "Error parsing DeprovisionPublicIpv4PoolCidrResult - missing field in \
                body or children: "
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
