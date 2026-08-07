open Aws.BaseTypes

type t =
  { enabled : Boolean.t option
  ; lambda_function_arn : String.t option
  ; status : ClientVpnEndpointAttributeStatus.t option
  }

let make ?enabled ?lambda_function_arn ?status () =
  { enabled; lambda_function_arn; status }

let parse xml =
  Some
    { enabled = Aws.Util.option_bind (Aws.Xml.member "enabled" xml) Boolean.parse
    ; lambda_function_arn =
        Aws.Util.option_bind (Aws.Xml.member "lambdaFunctionArn" xml) String.parse
    ; status =
        Aws.Util.option_bind
          (Aws.Xml.member "status" xml)
          ClientVpnEndpointAttributeStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ClientVpnEndpointAttributeStatus.to_query f))
       ; Aws.Util.option_map v.lambda_function_arn (fun f ->
             Aws.Query.Pair ("LambdaFunctionArn", String.to_query f))
       ; Aws.Util.option_map v.enabled (fun f ->
             Aws.Query.Pair ("Enabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             "status", ClientVpnEndpointAttributeStatus.to_json f)
       ; Aws.Util.option_map v.lambda_function_arn (fun f ->
             "lambdaFunctionArn", String.to_json f)
       ; Aws.Util.option_map v.enabled (fun f -> "enabled", Boolean.to_json f)
       ])

let of_json j =
  { enabled = Aws.Util.option_map (Aws.Json.lookup j "enabled") Boolean.of_json
  ; lambda_function_arn =
      Aws.Util.option_map (Aws.Json.lookup j "lambdaFunctionArn") String.of_json
  ; status =
      Aws.Util.option_map
        (Aws.Json.lookup j "status")
        ClientVpnEndpointAttributeStatus.of_json
  }
