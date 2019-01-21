open Types_internal
open Aws
type input = CreateDBSubnetGroupMessage.t
type output = CreateDBSubnetGroupResult.t
type error = Errors_internal.t
let service = "rds" 
let to_http req =
  let uri =
    Uri.add_query_params (Uri.of_string "https://rds.amazonaws.com")
      (List.append
         [("Version", ["2014-10-31"]); ("Action", ["CreateDBSubnetGroup"])]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (CreateDBSubnetGroupMessage.to_query req)))))
     in
  (`POST, uri, []) 
let of_http body =
  try
    let xml = Ezxmlm.from_string body  in
    let resp =
      Util.option_bind (Xml.member "CreateDBSubnetGroupResponse" (snd xml))
        (Xml.member "CreateDBSubnetGroupResult")
       in
    try
      Util.or_error (Util.option_bind resp CreateDBSubnetGroupResult.parse)
        (let open Error in
           BadResponse
             {
               body;
               message =
                 "Could not find well formed CreateDBSubnetGroupResult."
             })
    with
    | Xml.RequiredFieldMissing msg ->
        let open Error in
          `Error
            (BadResponse
               {
                 body;
                 message =
                   ("Error parsing CreateDBSubnetGroupResult - missing field in body or children: "
                      ^ msg)
               })
  with
  | Failure msg ->
      `Error
        (let open Error in
           BadResponse { body; message = ("Error parsing xml: " ^ msg) })
  
let parse_error code err =
  let errors =
    [Errors_internal.InvalidSubnet;
    Errors_internal.DBSubnetGroupDoesNotCoverEnoughAZs;
    Errors_internal.DBSubnetQuotaExceededFault;
    Errors_internal.DBSubnetGroupQuotaExceeded;
    Errors_internal.DBSubnetGroupAlreadyExists] @ Errors_internal.common  in
  match Errors_internal.of_string err with
  | Some var ->
      if
        (List.mem var errors) &&
          ((match Errors_internal.to_http_code var with
            | Some var -> var = code
            | None  -> true))
      then Some var
      else None
  | None  -> None 