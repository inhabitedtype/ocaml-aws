open Aws.BaseTypes

type t =
  { invocation_point : HookInvocationPoint.t option
  ; failure_mode : HookFailureMode.t option
  ; type_name : String.t option
  ; type_version_id : String.t option
  ; type_configuration_version_id : String.t option
  ; target_details : ChangeSetHookTargetDetails.t option
  }

let make
    ?invocation_point
    ?failure_mode
    ?type_name
    ?type_version_id
    ?type_configuration_version_id
    ?target_details
    () =
  { invocation_point
  ; failure_mode
  ; type_name
  ; type_version_id
  ; type_configuration_version_id
  ; target_details
  }

let parse xml =
  Some
    { invocation_point =
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
    ; target_details =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetDetails" xml)
          ChangeSetHookTargetDetails.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_details (fun f ->
             Aws.Query.Pair ("TargetDetails", ChangeSetHookTargetDetails.to_query f))
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
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_details (fun f ->
             "TargetDetails", ChangeSetHookTargetDetails.to_json f)
       ; Aws.Util.option_map v.type_configuration_version_id (fun f ->
             "TypeConfigurationVersionId", String.to_json f)
       ; Aws.Util.option_map v.type_version_id (fun f ->
             "TypeVersionId", String.to_json f)
       ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
       ; Aws.Util.option_map v.failure_mode (fun f ->
             "FailureMode", HookFailureMode.to_json f)
       ; Aws.Util.option_map v.invocation_point (fun f ->
             "InvocationPoint", HookInvocationPoint.to_json f)
       ])

let of_json j =
  { invocation_point =
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
  ; target_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "TargetDetails")
        ChangeSetHookTargetDetails.of_json
  }
