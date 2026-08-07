open Aws.BaseTypes

type t =
  { hook_result_id : String.t option
  ; invocation_point : HookInvocationPoint.t option
  ; failure_mode : HookFailureMode.t option
  ; type_name : String.t option
  ; type_version_id : String.t option
  ; type_configuration_version_id : String.t option
  ; status : HookStatus.t option
  ; hook_status_reason : String.t option
  ; invoked_at : DateTime.t option
  ; target_type : ListHookResultsTargetType.t option
  ; target_id : String.t option
  ; type_arn : String.t option
  ; hook_execution_target : String.t option
  }

let make
    ?hook_result_id
    ?invocation_point
    ?failure_mode
    ?type_name
    ?type_version_id
    ?type_configuration_version_id
    ?status
    ?hook_status_reason
    ?invoked_at
    ?target_type
    ?target_id
    ?type_arn
    ?hook_execution_target
    () =
  { hook_result_id
  ; invocation_point
  ; failure_mode
  ; type_name
  ; type_version_id
  ; type_configuration_version_id
  ; status
  ; hook_status_reason
  ; invoked_at
  ; target_type
  ; target_id
  ; type_arn
  ; hook_execution_target
  }

let parse xml =
  Some
    { hook_result_id =
        Aws.Util.option_bind (Aws.Xml.member "HookResultId" xml) String.parse
    ; invocation_point =
        Aws.Util.option_bind
          (Aws.Xml.member "InvocationPoint" xml)
          HookInvocationPoint.parse
    ; failure_mode =
        Aws.Util.option_bind (Aws.Xml.member "FailureMode" xml) HookFailureMode.parse
    ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
    ; type_version_id =
        Aws.Util.option_bind (Aws.Xml.member "TypeVersionId" xml) String.parse
    ; type_configuration_version_id =
        Aws.Util.option_bind
          (Aws.Xml.member "TypeConfigurationVersionId" xml)
          String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) HookStatus.parse
    ; hook_status_reason =
        Aws.Util.option_bind (Aws.Xml.member "HookStatusReason" xml) String.parse
    ; invoked_at = Aws.Util.option_bind (Aws.Xml.member "InvokedAt" xml) DateTime.parse
    ; target_type =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetType" xml)
          ListHookResultsTargetType.parse
    ; target_id = Aws.Util.option_bind (Aws.Xml.member "TargetId" xml) String.parse
    ; type_arn = Aws.Util.option_bind (Aws.Xml.member "TypeArn" xml) String.parse
    ; hook_execution_target =
        Aws.Util.option_bind (Aws.Xml.member "HookExecutionTarget" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.hook_execution_target (fun f ->
             Aws.Query.Pair ("HookExecutionTarget", String.to_query f))
       ; Aws.Util.option_map v.type_arn (fun f ->
             Aws.Query.Pair ("TypeArn", String.to_query f))
       ; Aws.Util.option_map v.target_id (fun f ->
             Aws.Query.Pair ("TargetId", String.to_query f))
       ; Aws.Util.option_map v.target_type (fun f ->
             Aws.Query.Pair ("TargetType", ListHookResultsTargetType.to_query f))
       ; Aws.Util.option_map v.invoked_at (fun f ->
             Aws.Query.Pair ("InvokedAt", DateTime.to_query f))
       ; Aws.Util.option_map v.hook_status_reason (fun f ->
             Aws.Query.Pair ("HookStatusReason", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", HookStatus.to_query f))
       ; Aws.Util.option_map v.type_configuration_version_id (fun f ->
             Aws.Query.Pair ("TypeConfigurationVersionId", String.to_query f))
       ; Aws.Util.option_map v.type_version_id (fun f ->
             Aws.Query.Pair ("TypeVersionId", String.to_query f))
       ; Aws.Util.option_map v.type_name (fun f ->
             Aws.Query.Pair ("TypeName", String.to_query f))
       ; Aws.Util.option_map v.failure_mode (fun f ->
             Aws.Query.Pair ("FailureMode", HookFailureMode.to_query f))
       ; Aws.Util.option_map v.invocation_point (fun f ->
             Aws.Query.Pair ("InvocationPoint", HookInvocationPoint.to_query f))
       ; Aws.Util.option_map v.hook_result_id (fun f ->
             Aws.Query.Pair ("HookResultId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.hook_execution_target (fun f ->
             "HookExecutionTarget", String.to_json f)
       ; Aws.Util.option_map v.type_arn (fun f -> "TypeArn", String.to_json f)
       ; Aws.Util.option_map v.target_id (fun f -> "TargetId", String.to_json f)
       ; Aws.Util.option_map v.target_type (fun f ->
             "TargetType", ListHookResultsTargetType.to_json f)
       ; Aws.Util.option_map v.invoked_at (fun f -> "InvokedAt", DateTime.to_json f)
       ; Aws.Util.option_map v.hook_status_reason (fun f ->
             "HookStatusReason", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", HookStatus.to_json f)
       ; Aws.Util.option_map v.type_configuration_version_id (fun f ->
             "TypeConfigurationVersionId", String.to_json f)
       ; Aws.Util.option_map v.type_version_id (fun f ->
             "TypeVersionId", String.to_json f)
       ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
       ; Aws.Util.option_map v.failure_mode (fun f ->
             "FailureMode", HookFailureMode.to_json f)
       ; Aws.Util.option_map v.invocation_point (fun f ->
             "InvocationPoint", HookInvocationPoint.to_json f)
       ; Aws.Util.option_map v.hook_result_id (fun f -> "HookResultId", String.to_json f)
       ])

let of_json j =
  { hook_result_id = Aws.Util.option_map (Aws.Json.lookup j "HookResultId") String.of_json
  ; invocation_point =
      Aws.Util.option_map
        (Aws.Json.lookup j "InvocationPoint")
        HookInvocationPoint.of_json
  ; failure_mode =
      Aws.Util.option_map (Aws.Json.lookup j "FailureMode") HookFailureMode.of_json
  ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
  ; type_version_id =
      Aws.Util.option_map (Aws.Json.lookup j "TypeVersionId") String.of_json
  ; type_configuration_version_id =
      Aws.Util.option_map (Aws.Json.lookup j "TypeConfigurationVersionId") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") HookStatus.of_json
  ; hook_status_reason =
      Aws.Util.option_map (Aws.Json.lookup j "HookStatusReason") String.of_json
  ; invoked_at = Aws.Util.option_map (Aws.Json.lookup j "InvokedAt") DateTime.of_json
  ; target_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "TargetType")
        ListHookResultsTargetType.of_json
  ; target_id = Aws.Util.option_map (Aws.Json.lookup j "TargetId") String.of_json
  ; type_arn = Aws.Util.option_map (Aws.Json.lookup j "TypeArn") String.of_json
  ; hook_execution_target =
      Aws.Util.option_map (Aws.Json.lookup j "HookExecutionTarget") String.of_json
  }
