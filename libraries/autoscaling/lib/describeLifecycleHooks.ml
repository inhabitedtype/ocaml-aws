open Types
open Aws
type input = DescribeLifecycleHooksType.t
type output = DescribeLifecycleHooksAnswer.t
type error = Errors.t
let service = "autoscaling" 
let to_http req =
  let uri =
    Uri.add_query_params (Uri.of_string "https://autoscaling.amazonaws.com")
      (List.append
         [("Version", ["2011-01-01"]);
         ("Action", ["DescribeLifecycleHooks"])]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (DescribeLifecycleHooksType.to_query req)))))
     in
  (`POST, uri, []) 
let of_http body =
  try
    let xml = Ezxmlm.from_string body  in
    let resp =
      Util.option_bind
        (Xml.member "DescribeLifecycleHooksResponse" (snd xml))
        (Xml.member "DescribeLifecycleHooksResult")
       in
    try
      Util.or_error
        (Util.option_bind resp DescribeLifecycleHooksAnswer.parse)
        (let open Error in
           BadResponse
             {
               body;
               message =
                 "Could not find well formed DescribeLifecycleHooksAnswer."
             })
    with
    | Xml.RequiredFieldMissing msg ->
        let open Error in
          `Error
            (BadResponse
               {
                 body;
                 message =
                   ("Error parsing DescribeLifecycleHooksAnswer - missing field in body or children: "
                      ^ msg)
               })
  with
  | Failure msg ->
      `Error
        (let open Error in
           BadResponse { body; message = ("Error parsing xml: " ^ msg) })
  
let parse_error code err =
  let errors = [Errors.ResourceContention] @ Errors.common  in
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