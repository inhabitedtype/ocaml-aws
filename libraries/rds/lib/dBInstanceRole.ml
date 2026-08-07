open Aws.BaseTypes

type t =
  { role_arn : String.t option
  ; feature_name : String.t option
  ; status : String.t option
  }

let make ?role_arn ?feature_name ?status () = { role_arn; feature_name; status }

let parse xml =
  Some
    { role_arn = Aws.Util.option_bind (Aws.Xml.member "RoleArn" xml) String.parse
    ; feature_name = Aws.Util.option_bind (Aws.Xml.member "FeatureName" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.feature_name (fun f ->
             Aws.Query.Pair ("FeatureName", String.to_query f))
       ; Aws.Util.option_map v.role_arn (fun f ->
             Aws.Query.Pair ("RoleArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.feature_name (fun f -> "FeatureName", String.to_json f)
       ; Aws.Util.option_map v.role_arn (fun f -> "RoleArn", String.to_json f)
       ])

let of_json j =
  { role_arn = Aws.Util.option_map (Aws.Json.lookup j "RoleArn") String.of_json
  ; feature_name = Aws.Util.option_map (Aws.Json.lookup j "FeatureName") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  }
