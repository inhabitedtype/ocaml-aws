open Aws.BaseTypes

type t =
  { stack_set_id : String.t option
  ; region : String.t option
  ; account : String.t option
  ; stack_id : String.t option
  ; parameter_overrides : Parameters.t
  ; status : StackInstanceStatus.t option
  ; stack_instance_status : StackInstanceComprehensiveStatus.t option
  ; status_reason : String.t option
  ; organizational_unit_id : String.t option
  ; drift_status : StackDriftStatus.t option
  ; last_drift_check_timestamp : DateTime.t option
  ; last_operation_id : String.t option
  }

let make
    ?stack_set_id
    ?region
    ?account
    ?stack_id
    ?(parameter_overrides = [])
    ?status
    ?stack_instance_status
    ?status_reason
    ?organizational_unit_id
    ?drift_status
    ?last_drift_check_timestamp
    ?last_operation_id
    () =
  { stack_set_id
  ; region
  ; account
  ; stack_id
  ; parameter_overrides
  ; status
  ; stack_instance_status
  ; status_reason
  ; organizational_unit_id
  ; drift_status
  ; last_drift_check_timestamp
  ; last_operation_id
  }

let parse xml =
  Some
    { stack_set_id = Aws.Util.option_bind (Aws.Xml.member "StackSetId" xml) String.parse
    ; region = Aws.Util.option_bind (Aws.Xml.member "Region" xml) String.parse
    ; account = Aws.Util.option_bind (Aws.Xml.member "Account" xml) String.parse
    ; stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
    ; parameter_overrides =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ParameterOverrides" xml)
             Parameters.parse)
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) StackInstanceStatus.parse
    ; stack_instance_status =
        Aws.Util.option_bind
          (Aws.Xml.member "StackInstanceStatus" xml)
          StackInstanceComprehensiveStatus.parse
    ; status_reason =
        Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
    ; organizational_unit_id =
        Aws.Util.option_bind (Aws.Xml.member "OrganizationalUnitId" xml) String.parse
    ; drift_status =
        Aws.Util.option_bind (Aws.Xml.member "DriftStatus" xml) StackDriftStatus.parse
    ; last_drift_check_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "LastDriftCheckTimestamp" xml) DateTime.parse
    ; last_operation_id =
        Aws.Util.option_bind (Aws.Xml.member "LastOperationId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_operation_id (fun f ->
             Aws.Query.Pair ("LastOperationId", String.to_query f))
       ; Aws.Util.option_map v.last_drift_check_timestamp (fun f ->
             Aws.Query.Pair ("LastDriftCheckTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.drift_status (fun f ->
             Aws.Query.Pair ("DriftStatus", StackDriftStatus.to_query f))
       ; Aws.Util.option_map v.organizational_unit_id (fun f ->
             Aws.Query.Pair ("OrganizationalUnitId", String.to_query f))
       ; Aws.Util.option_map v.status_reason (fun f ->
             Aws.Query.Pair ("StatusReason", String.to_query f))
       ; Aws.Util.option_map v.stack_instance_status (fun f ->
             Aws.Query.Pair
               ("StackInstanceStatus", StackInstanceComprehensiveStatus.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", StackInstanceStatus.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ParameterOverrides.member", Parameters.to_query v.parameter_overrides))
       ; Aws.Util.option_map v.stack_id (fun f ->
             Aws.Query.Pair ("StackId", String.to_query f))
       ; Aws.Util.option_map v.account (fun f ->
             Aws.Query.Pair ("Account", String.to_query f))
       ; Aws.Util.option_map v.region (fun f ->
             Aws.Query.Pair ("Region", String.to_query f))
       ; Aws.Util.option_map v.stack_set_id (fun f ->
             Aws.Query.Pair ("StackSetId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_operation_id (fun f ->
             "LastOperationId", String.to_json f)
       ; Aws.Util.option_map v.last_drift_check_timestamp (fun f ->
             "LastDriftCheckTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.drift_status (fun f ->
             "DriftStatus", StackDriftStatus.to_json f)
       ; Aws.Util.option_map v.organizational_unit_id (fun f ->
             "OrganizationalUnitId", String.to_json f)
       ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
       ; Aws.Util.option_map v.stack_instance_status (fun f ->
             "StackInstanceStatus", StackInstanceComprehensiveStatus.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", StackInstanceStatus.to_json f)
       ; Some ("ParameterOverrides", Parameters.to_json v.parameter_overrides)
       ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
       ; Aws.Util.option_map v.account (fun f -> "Account", String.to_json f)
       ; Aws.Util.option_map v.region (fun f -> "Region", String.to_json f)
       ; Aws.Util.option_map v.stack_set_id (fun f -> "StackSetId", String.to_json f)
       ])

let of_json j =
  { stack_set_id = Aws.Util.option_map (Aws.Json.lookup j "StackSetId") String.of_json
  ; region = Aws.Util.option_map (Aws.Json.lookup j "Region") String.of_json
  ; account = Aws.Util.option_map (Aws.Json.lookup j "Account") String.of_json
  ; stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
  ; parameter_overrides =
      Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ParameterOverrides"))
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") StackInstanceStatus.of_json
  ; stack_instance_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "StackInstanceStatus")
        StackInstanceComprehensiveStatus.of_json
  ; status_reason = Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
  ; organizational_unit_id =
      Aws.Util.option_map (Aws.Json.lookup j "OrganizationalUnitId") String.of_json
  ; drift_status =
      Aws.Util.option_map (Aws.Json.lookup j "DriftStatus") StackDriftStatus.of_json
  ; last_drift_check_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "LastDriftCheckTimestamp") DateTime.of_json
  ; last_operation_id =
      Aws.Util.option_map (Aws.Json.lookup j "LastOperationId") String.of_json
  }
