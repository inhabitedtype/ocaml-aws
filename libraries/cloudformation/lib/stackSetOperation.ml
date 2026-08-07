open Aws.BaseTypes

type t =
  { operation_id : String.t option
  ; stack_set_id : String.t option
  ; action : StackSetOperationAction.t option
  ; status : StackSetOperationStatus.t option
  ; operation_preferences : StackSetOperationPreferences.t option
  ; retain_stacks : Boolean.t option
  ; administration_role_a_r_n : String.t option
  ; execution_role_name : String.t option
  ; creation_timestamp : DateTime.t option
  ; end_timestamp : DateTime.t option
  ; deployment_targets : DeploymentTargets.t option
  ; stack_set_drift_detection_details : StackSetDriftDetectionDetails.t option
  ; status_reason : String.t option
  ; status_details : StackSetOperationStatusDetails.t option
  }

let make
    ?operation_id
    ?stack_set_id
    ?action
    ?status
    ?operation_preferences
    ?retain_stacks
    ?administration_role_a_r_n
    ?execution_role_name
    ?creation_timestamp
    ?end_timestamp
    ?deployment_targets
    ?stack_set_drift_detection_details
    ?status_reason
    ?status_details
    () =
  { operation_id
  ; stack_set_id
  ; action
  ; status
  ; operation_preferences
  ; retain_stacks
  ; administration_role_a_r_n
  ; execution_role_name
  ; creation_timestamp
  ; end_timestamp
  ; deployment_targets
  ; stack_set_drift_detection_details
  ; status_reason
  ; status_details
  }

let parse xml =
  Some
    { operation_id = Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
    ; stack_set_id = Aws.Util.option_bind (Aws.Xml.member "StackSetId" xml) String.parse
    ; action =
        Aws.Util.option_bind (Aws.Xml.member "Action" xml) StackSetOperationAction.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) StackSetOperationStatus.parse
    ; operation_preferences =
        Aws.Util.option_bind
          (Aws.Xml.member "OperationPreferences" xml)
          StackSetOperationPreferences.parse
    ; retain_stacks =
        Aws.Util.option_bind (Aws.Xml.member "RetainStacks" xml) Boolean.parse
    ; administration_role_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "AdministrationRoleARN" xml) String.parse
    ; execution_role_name =
        Aws.Util.option_bind (Aws.Xml.member "ExecutionRoleName" xml) String.parse
    ; creation_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "CreationTimestamp" xml) DateTime.parse
    ; end_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "EndTimestamp" xml) DateTime.parse
    ; deployment_targets =
        Aws.Util.option_bind
          (Aws.Xml.member "DeploymentTargets" xml)
          DeploymentTargets.parse
    ; stack_set_drift_detection_details =
        Aws.Util.option_bind
          (Aws.Xml.member "StackSetDriftDetectionDetails" xml)
          StackSetDriftDetectionDetails.parse
    ; status_reason =
        Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
    ; status_details =
        Aws.Util.option_bind
          (Aws.Xml.member "StatusDetails" xml)
          StackSetOperationStatusDetails.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status_details (fun f ->
             Aws.Query.Pair ("StatusDetails", StackSetOperationStatusDetails.to_query f))
       ; Aws.Util.option_map v.status_reason (fun f ->
             Aws.Query.Pair ("StatusReason", String.to_query f))
       ; Aws.Util.option_map v.stack_set_drift_detection_details (fun f ->
             Aws.Query.Pair
               ("StackSetDriftDetectionDetails", StackSetDriftDetectionDetails.to_query f))
       ; Aws.Util.option_map v.deployment_targets (fun f ->
             Aws.Query.Pair ("DeploymentTargets", DeploymentTargets.to_query f))
       ; Aws.Util.option_map v.end_timestamp (fun f ->
             Aws.Query.Pair ("EndTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.creation_timestamp (fun f ->
             Aws.Query.Pair ("CreationTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.execution_role_name (fun f ->
             Aws.Query.Pair ("ExecutionRoleName", String.to_query f))
       ; Aws.Util.option_map v.administration_role_a_r_n (fun f ->
             Aws.Query.Pair ("AdministrationRoleARN", String.to_query f))
       ; Aws.Util.option_map v.retain_stacks (fun f ->
             Aws.Query.Pair ("RetainStacks", Boolean.to_query f))
       ; Aws.Util.option_map v.operation_preferences (fun f ->
             Aws.Query.Pair
               ("OperationPreferences", StackSetOperationPreferences.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", StackSetOperationStatus.to_query f))
       ; Aws.Util.option_map v.action (fun f ->
             Aws.Query.Pair ("Action", StackSetOperationAction.to_query f))
       ; Aws.Util.option_map v.stack_set_id (fun f ->
             Aws.Query.Pair ("StackSetId", String.to_query f))
       ; Aws.Util.option_map v.operation_id (fun f ->
             Aws.Query.Pair ("OperationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status_details (fun f ->
             "StatusDetails", StackSetOperationStatusDetails.to_json f)
       ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
       ; Aws.Util.option_map v.stack_set_drift_detection_details (fun f ->
             "StackSetDriftDetectionDetails", StackSetDriftDetectionDetails.to_json f)
       ; Aws.Util.option_map v.deployment_targets (fun f ->
             "DeploymentTargets", DeploymentTargets.to_json f)
       ; Aws.Util.option_map v.end_timestamp (fun f -> "EndTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.creation_timestamp (fun f ->
             "CreationTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.execution_role_name (fun f ->
             "ExecutionRoleName", String.to_json f)
       ; Aws.Util.option_map v.administration_role_a_r_n (fun f ->
             "AdministrationRoleARN", String.to_json f)
       ; Aws.Util.option_map v.retain_stacks (fun f -> "RetainStacks", Boolean.to_json f)
       ; Aws.Util.option_map v.operation_preferences (fun f ->
             "OperationPreferences", StackSetOperationPreferences.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "Status", StackSetOperationStatus.to_json f)
       ; Aws.Util.option_map v.action (fun f ->
             "Action", StackSetOperationAction.to_json f)
       ; Aws.Util.option_map v.stack_set_id (fun f -> "StackSetId", String.to_json f)
       ; Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
       ])

let of_json j =
  { operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
  ; stack_set_id = Aws.Util.option_map (Aws.Json.lookup j "StackSetId") String.of_json
  ; action =
      Aws.Util.option_map (Aws.Json.lookup j "Action") StackSetOperationAction.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "Status") StackSetOperationStatus.of_json
  ; operation_preferences =
      Aws.Util.option_map
        (Aws.Json.lookup j "OperationPreferences")
        StackSetOperationPreferences.of_json
  ; retain_stacks = Aws.Util.option_map (Aws.Json.lookup j "RetainStacks") Boolean.of_json
  ; administration_role_a_r_n =
      Aws.Util.option_map (Aws.Json.lookup j "AdministrationRoleARN") String.of_json
  ; execution_role_name =
      Aws.Util.option_map (Aws.Json.lookup j "ExecutionRoleName") String.of_json
  ; creation_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "CreationTimestamp") DateTime.of_json
  ; end_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "EndTimestamp") DateTime.of_json
  ; deployment_targets =
      Aws.Util.option_map
        (Aws.Json.lookup j "DeploymentTargets")
        DeploymentTargets.of_json
  ; stack_set_drift_detection_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "StackSetDriftDetectionDetails")
        StackSetDriftDetectionDetails.of_json
  ; status_reason = Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
  ; status_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "StatusDetails")
        StackSetOperationStatusDetails.of_json
  }
