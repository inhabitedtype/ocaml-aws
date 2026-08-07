open Aws.BaseTypes

type t =
  { stack_set_name : String.t option
  ; stack_set_id : String.t option
  ; description : String.t option
  ; status : StackSetStatus.t option
  ; auto_deployment : AutoDeployment.t option
  ; permission_model : PermissionModels.t option
  ; drift_status : StackDriftStatus.t option
  ; last_drift_check_timestamp : DateTime.t option
  ; managed_execution : ManagedExecution.t option
  }

let make
    ?stack_set_name
    ?stack_set_id
    ?description
    ?status
    ?auto_deployment
    ?permission_model
    ?drift_status
    ?last_drift_check_timestamp
    ?managed_execution
    () =
  { stack_set_name
  ; stack_set_id
  ; description
  ; status
  ; auto_deployment
  ; permission_model
  ; drift_status
  ; last_drift_check_timestamp
  ; managed_execution
  }

let parse xml =
  Some
    { stack_set_name =
        Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse
    ; stack_set_id = Aws.Util.option_bind (Aws.Xml.member "StackSetId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) StackSetStatus.parse
    ; auto_deployment =
        Aws.Util.option_bind (Aws.Xml.member "AutoDeployment" xml) AutoDeployment.parse
    ; permission_model =
        Aws.Util.option_bind (Aws.Xml.member "PermissionModel" xml) PermissionModels.parse
    ; drift_status =
        Aws.Util.option_bind (Aws.Xml.member "DriftStatus" xml) StackDriftStatus.parse
    ; last_drift_check_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "LastDriftCheckTimestamp" xml) DateTime.parse
    ; managed_execution =
        Aws.Util.option_bind
          (Aws.Xml.member "ManagedExecution" xml)
          ManagedExecution.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_execution (fun f ->
             Aws.Query.Pair ("ManagedExecution", ManagedExecution.to_query f))
       ; Aws.Util.option_map v.last_drift_check_timestamp (fun f ->
             Aws.Query.Pair ("LastDriftCheckTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.drift_status (fun f ->
             Aws.Query.Pair ("DriftStatus", StackDriftStatus.to_query f))
       ; Aws.Util.option_map v.permission_model (fun f ->
             Aws.Query.Pair ("PermissionModel", PermissionModels.to_query f))
       ; Aws.Util.option_map v.auto_deployment (fun f ->
             Aws.Query.Pair ("AutoDeployment", AutoDeployment.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", StackSetStatus.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.stack_set_id (fun f ->
             Aws.Query.Pair ("StackSetId", String.to_query f))
       ; Aws.Util.option_map v.stack_set_name (fun f ->
             Aws.Query.Pair ("StackSetName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_execution (fun f ->
             "ManagedExecution", ManagedExecution.to_json f)
       ; Aws.Util.option_map v.last_drift_check_timestamp (fun f ->
             "LastDriftCheckTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.drift_status (fun f ->
             "DriftStatus", StackDriftStatus.to_json f)
       ; Aws.Util.option_map v.permission_model (fun f ->
             "PermissionModel", PermissionModels.to_json f)
       ; Aws.Util.option_map v.auto_deployment (fun f ->
             "AutoDeployment", AutoDeployment.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", StackSetStatus.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.stack_set_id (fun f -> "StackSetId", String.to_json f)
       ; Aws.Util.option_map v.stack_set_name (fun f -> "StackSetName", String.to_json f)
       ])

let of_json j =
  { stack_set_name = Aws.Util.option_map (Aws.Json.lookup j "StackSetName") String.of_json
  ; stack_set_id = Aws.Util.option_map (Aws.Json.lookup j "StackSetId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") StackSetStatus.of_json
  ; auto_deployment =
      Aws.Util.option_map (Aws.Json.lookup j "AutoDeployment") AutoDeployment.of_json
  ; permission_model =
      Aws.Util.option_map (Aws.Json.lookup j "PermissionModel") PermissionModels.of_json
  ; drift_status =
      Aws.Util.option_map (Aws.Json.lookup j "DriftStatus") StackDriftStatus.of_json
  ; last_drift_check_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "LastDriftCheckTimestamp") DateTime.of_json
  ; managed_execution =
      Aws.Util.option_map (Aws.Json.lookup j "ManagedExecution") ManagedExecution.of_json
  }
