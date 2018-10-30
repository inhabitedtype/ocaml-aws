open Types
open Aws
type input = SelectRequest.t
type output = SelectResult.t
type error = Errors.t
let service = "sdb" 
let to_http req =
  let uri =
    Uri.add_query_params (Uri.of_string "https://sdb.amazonaws.com")
      (List.append [("Version", ["2009-04-15"]); ("Action", ["Select"])]
         (Util.drop_empty
            (Uri.query_of_encoded (Query.render (SelectRequest.to_query req)))))
     in
  (`POST, uri, []) 
let of_http body =
  try
    let xml = Ezxmlm.from_string body  in
    let resp =
      Util.option_bind (Xml.member "SelectResponse" (snd xml))
        (Xml.member "SelectResult")
       in
    try
      Util.or_error (Util.option_bind resp SelectResult.parse)
        (let open Error in
           BadResponse
             { body; message = "Could not find well formed SelectResult." })
    with
    | Xml.RequiredFieldMissing msg ->
        let open Error in
          `Error
            (BadResponse
               {
                 body;
                 message =
                   ("Error parsing SelectResult - missing field in body or children: "
                      ^ msg)
               })
  with
  | Failure msg ->
      `Error
        (let open Error in
           BadResponse { body; message = ("Error parsing xml: " ^ msg) })
  
let parse_error code err =
  let errors =
    [Errors.TooManyRequestedAttributes;
    Errors.RequestTimeout;
    Errors.NoSuchDomain;
    Errors.MissingParameter;
    Errors.InvalidQueryExpression;
    Errors.InvalidNumberValueTests;
    Errors.InvalidNumberPredicates;
    Errors.InvalidNextToken;
    Errors.InvalidParameterValue] @ Errors.common  in
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