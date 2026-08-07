open Aws.BaseTypes

type t =
  { target_type : ListHookResultsTargetType.t option
  ; target_id : String.t option
  ; type_arn : String.t option
  ; status : HookStatus.t option
  ; next_token : String.t option
  }

let make ?target_type ?target_id ?type_arn ?status ?next_token () =
  { target_type; target_id; type_arn; status; next_token }

let parse xml =
  Some
    { target_type =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetType" xml)
          ListHookResultsTargetType.parse
    ; target_id = Aws.Util.option_bind (Aws.Xml.member "TargetId" xml) String.parse
    ; type_arn = Aws.Util.option_bind (Aws.Xml.member "TypeArn" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) HookStatus.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", HookStatus.to_query f))
       ; Aws.Util.option_map v.type_arn (fun f ->
             Aws.Query.Pair ("TypeArn", String.to_query f))
       ; Aws.Util.option_map v.target_id (fun f ->
             Aws.Query.Pair ("TargetId", String.to_query f))
       ; Aws.Util.option_map v.target_type (fun f ->
             Aws.Query.Pair ("TargetType", ListHookResultsTargetType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", HookStatus.to_json f)
       ; Aws.Util.option_map v.type_arn (fun f -> "TypeArn", String.to_json f)
       ; Aws.Util.option_map v.target_id (fun f -> "TargetId", String.to_json f)
       ; Aws.Util.option_map v.target_type (fun f ->
             "TargetType", ListHookResultsTargetType.to_json f)
       ])

let of_json j =
  { target_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "TargetType")
        ListHookResultsTargetType.of_json
  ; target_id = Aws.Util.option_map (Aws.Json.lookup j "TargetId") String.of_json
  ; type_arn = Aws.Util.option_map (Aws.Json.lookup j "TypeArn") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") HookStatus.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
