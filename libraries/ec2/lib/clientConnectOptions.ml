open Aws.BaseTypes

type t =
  { enabled : Boolean.t option
  ; lambda_function_arn : String.t option
  }

let make ?enabled ?lambda_function_arn () = { enabled; lambda_function_arn }

let parse xml =
  Some
    { enabled = Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse
    ; lambda_function_arn =
        Aws.Util.option_bind (Aws.Xml.member "LambdaFunctionArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.lambda_function_arn (fun f ->
             Aws.Query.Pair ("LambdaFunctionArn", String.to_query f))
       ; Aws.Util.option_map v.enabled (fun f ->
             Aws.Query.Pair ("Enabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.lambda_function_arn (fun f ->
             "LambdaFunctionArn", String.to_json f)
       ; Aws.Util.option_map v.enabled (fun f -> "Enabled", Boolean.to_json f)
       ])

let of_json j =
  { enabled = Aws.Util.option_map (Aws.Json.lookup j "Enabled") Boolean.of_json
  ; lambda_function_arn =
      Aws.Util.option_map (Aws.Json.lookup j "LambdaFunctionArn") String.of_json
  }
