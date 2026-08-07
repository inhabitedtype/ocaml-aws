open Aws.BaseTypes

type t =
  { event_id : String.t option
  ; stack_id : String.t option
  ; operation_id : String.t option
  ; operation_type : OperationType.t option
  ; operation_status : BeaconStackOperationStatus.t option
  ; event_type : EventType.t option
  ; logical_resource_id : String.t option
  ; physical_resource_id : String.t option
  ; resource_type : String.t option
  ; timestamp : DateTime.t option
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; resource_status : ResourceStatus.t option
  ; resource_status_reason : String.t option
  ; resource_properties : String.t option
  ; client_request_token : String.t option
  ; hook_type : String.t option
  ; hook_status : HookStatus.t option
  ; hook_status_reason : String.t option
  ; hook_invocation_point : HookInvocationPoint.t option
  ; hook_failure_mode : HookFailureMode.t option
  ; detailed_status : DetailedStatus.t option
  ; validation_failure_mode : HookFailureMode.t option
  ; validation_name : String.t option
  ; validation_status : ValidationStatus.t option
  ; validation_status_reason : String.t option
  ; validation_path : String.t option
  }

let make
    ?event_id
    ?stack_id
    ?operation_id
    ?operation_type
    ?operation_status
    ?event_type
    ?logical_resource_id
    ?physical_resource_id
    ?resource_type
    ?timestamp
    ?start_time
    ?end_time
    ?resource_status
    ?resource_status_reason
    ?resource_properties
    ?client_request_token
    ?hook_type
    ?hook_status
    ?hook_status_reason
    ?hook_invocation_point
    ?hook_failure_mode
    ?detailed_status
    ?validation_failure_mode
    ?validation_name
    ?validation_status
    ?validation_status_reason
    ?validation_path
    () =
  { event_id
  ; stack_id
  ; operation_id
  ; operation_type
  ; operation_status
  ; event_type
  ; logical_resource_id
  ; physical_resource_id
  ; resource_type
  ; timestamp
  ; start_time
  ; end_time
  ; resource_status
  ; resource_status_reason
  ; resource_properties
  ; client_request_token
  ; hook_type
  ; hook_status
  ; hook_status_reason
  ; hook_invocation_point
  ; hook_failure_mode
  ; detailed_status
  ; validation_failure_mode
  ; validation_name
  ; validation_status
  ; validation_status_reason
  ; validation_path
  }

let parse xml =
  Some
    { event_id = Aws.Util.option_bind (Aws.Xml.member "EventId" xml) String.parse
    ; stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
    ; operation_id = Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
    ; operation_type =
        Aws.Util.option_bind (Aws.Xml.member "OperationType" xml) OperationType.parse
    ; operation_status =
        Aws.Util.option_bind
          (Aws.Xml.member "OperationStatus" xml)
          BeaconStackOperationStatus.parse
    ; event_type = Aws.Util.option_bind (Aws.Xml.member "EventType" xml) EventType.parse
    ; logical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse
    ; physical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "PhysicalResourceId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
    ; timestamp = Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
    ; resource_status =
        Aws.Util.option_bind (Aws.Xml.member "ResourceStatus" xml) ResourceStatus.parse
    ; resource_status_reason =
        Aws.Util.option_bind (Aws.Xml.member "ResourceStatusReason" xml) String.parse
    ; resource_properties =
        Aws.Util.option_bind (Aws.Xml.member "ResourceProperties" xml) String.parse
    ; client_request_token =
        Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
    ; hook_type = Aws.Util.option_bind (Aws.Xml.member "HookType" xml) String.parse
    ; hook_status =
        Aws.Util.option_bind (Aws.Xml.member "HookStatus" xml) HookStatus.parse
    ; hook_status_reason =
        Aws.Util.option_bind (Aws.Xml.member "HookStatusReason" xml) String.parse
    ; hook_invocation_point =
        Aws.Util.option_bind
          (Aws.Xml.member "HookInvocationPoint" xml)
          HookInvocationPoint.parse
    ; hook_failure_mode =
        Aws.Util.option_bind (Aws.Xml.member "HookFailureMode" xml) HookFailureMode.parse
    ; detailed_status =
        Aws.Util.option_bind (Aws.Xml.member "DetailedStatus" xml) DetailedStatus.parse
    ; validation_failure_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "ValidationFailureMode" xml)
          HookFailureMode.parse
    ; validation_name =
        Aws.Util.option_bind (Aws.Xml.member "ValidationName" xml) String.parse
    ; validation_status =
        Aws.Util.option_bind
          (Aws.Xml.member "ValidationStatus" xml)
          ValidationStatus.parse
    ; validation_status_reason =
        Aws.Util.option_bind (Aws.Xml.member "ValidationStatusReason" xml) String.parse
    ; validation_path =
        Aws.Util.option_bind (Aws.Xml.member "ValidationPath" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.validation_path (fun f ->
             Aws.Query.Pair ("ValidationPath", String.to_query f))
       ; Aws.Util.option_map v.validation_status_reason (fun f ->
             Aws.Query.Pair ("ValidationStatusReason", String.to_query f))
       ; Aws.Util.option_map v.validation_status (fun f ->
             Aws.Query.Pair ("ValidationStatus", ValidationStatus.to_query f))
       ; Aws.Util.option_map v.validation_name (fun f ->
             Aws.Query.Pair ("ValidationName", String.to_query f))
       ; Aws.Util.option_map v.validation_failure_mode (fun f ->
             Aws.Query.Pair ("ValidationFailureMode", HookFailureMode.to_query f))
       ; Aws.Util.option_map v.detailed_status (fun f ->
             Aws.Query.Pair ("DetailedStatus", DetailedStatus.to_query f))
       ; Aws.Util.option_map v.hook_failure_mode (fun f ->
             Aws.Query.Pair ("HookFailureMode", HookFailureMode.to_query f))
       ; Aws.Util.option_map v.hook_invocation_point (fun f ->
             Aws.Query.Pair ("HookInvocationPoint", HookInvocationPoint.to_query f))
       ; Aws.Util.option_map v.hook_status_reason (fun f ->
             Aws.Query.Pair ("HookStatusReason", String.to_query f))
       ; Aws.Util.option_map v.hook_status (fun f ->
             Aws.Query.Pair ("HookStatus", HookStatus.to_query f))
       ; Aws.Util.option_map v.hook_type (fun f ->
             Aws.Query.Pair ("HookType", String.to_query f))
       ; Aws.Util.option_map v.client_request_token (fun f ->
             Aws.Query.Pair ("ClientRequestToken", String.to_query f))
       ; Aws.Util.option_map v.resource_properties (fun f ->
             Aws.Query.Pair ("ResourceProperties", String.to_query f))
       ; Aws.Util.option_map v.resource_status_reason (fun f ->
             Aws.Query.Pair ("ResourceStatusReason", String.to_query f))
       ; Aws.Util.option_map v.resource_status (fun f ->
             Aws.Query.Pair ("ResourceStatus", ResourceStatus.to_query f))
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.timestamp (fun f ->
             Aws.Query.Pair ("Timestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", String.to_query f))
       ; Aws.Util.option_map v.physical_resource_id (fun f ->
             Aws.Query.Pair ("PhysicalResourceId", String.to_query f))
       ; Aws.Util.option_map v.logical_resource_id (fun f ->
             Aws.Query.Pair ("LogicalResourceId", String.to_query f))
       ; Aws.Util.option_map v.event_type (fun f ->
             Aws.Query.Pair ("EventType", EventType.to_query f))
       ; Aws.Util.option_map v.operation_status (fun f ->
             Aws.Query.Pair ("OperationStatus", BeaconStackOperationStatus.to_query f))
       ; Aws.Util.option_map v.operation_type (fun f ->
             Aws.Query.Pair ("OperationType", OperationType.to_query f))
       ; Aws.Util.option_map v.operation_id (fun f ->
             Aws.Query.Pair ("OperationId", String.to_query f))
       ; Aws.Util.option_map v.stack_id (fun f ->
             Aws.Query.Pair ("StackId", String.to_query f))
       ; Aws.Util.option_map v.event_id (fun f ->
             Aws.Query.Pair ("EventId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.validation_path (fun f ->
             "ValidationPath", String.to_json f)
       ; Aws.Util.option_map v.validation_status_reason (fun f ->
             "ValidationStatusReason", String.to_json f)
       ; Aws.Util.option_map v.validation_status (fun f ->
             "ValidationStatus", ValidationStatus.to_json f)
       ; Aws.Util.option_map v.validation_name (fun f ->
             "ValidationName", String.to_json f)
       ; Aws.Util.option_map v.validation_failure_mode (fun f ->
             "ValidationFailureMode", HookFailureMode.to_json f)
       ; Aws.Util.option_map v.detailed_status (fun f ->
             "DetailedStatus", DetailedStatus.to_json f)
       ; Aws.Util.option_map v.hook_failure_mode (fun f ->
             "HookFailureMode", HookFailureMode.to_json f)
       ; Aws.Util.option_map v.hook_invocation_point (fun f ->
             "HookInvocationPoint", HookInvocationPoint.to_json f)
       ; Aws.Util.option_map v.hook_status_reason (fun f ->
             "HookStatusReason", String.to_json f)
       ; Aws.Util.option_map v.hook_status (fun f -> "HookStatus", HookStatus.to_json f)
       ; Aws.Util.option_map v.hook_type (fun f -> "HookType", String.to_json f)
       ; Aws.Util.option_map v.client_request_token (fun f ->
             "ClientRequestToken", String.to_json f)
       ; Aws.Util.option_map v.resource_properties (fun f ->
             "ResourceProperties", String.to_json f)
       ; Aws.Util.option_map v.resource_status_reason (fun f ->
             "ResourceStatusReason", String.to_json f)
       ; Aws.Util.option_map v.resource_status (fun f ->
             "ResourceStatus", ResourceStatus.to_json f)
       ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.timestamp (fun f -> "Timestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
       ; Aws.Util.option_map v.physical_resource_id (fun f ->
             "PhysicalResourceId", String.to_json f)
       ; Aws.Util.option_map v.logical_resource_id (fun f ->
             "LogicalResourceId", String.to_json f)
       ; Aws.Util.option_map v.event_type (fun f -> "EventType", EventType.to_json f)
       ; Aws.Util.option_map v.operation_status (fun f ->
             "OperationStatus", BeaconStackOperationStatus.to_json f)
       ; Aws.Util.option_map v.operation_type (fun f ->
             "OperationType", OperationType.to_json f)
       ; Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
       ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
       ; Aws.Util.option_map v.event_id (fun f -> "EventId", String.to_json f)
       ])

let of_json j =
  { event_id = Aws.Util.option_map (Aws.Json.lookup j "EventId") String.of_json
  ; stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
  ; operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
  ; operation_type =
      Aws.Util.option_map (Aws.Json.lookup j "OperationType") OperationType.of_json
  ; operation_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "OperationStatus")
        BeaconStackOperationStatus.of_json
  ; event_type = Aws.Util.option_map (Aws.Json.lookup j "EventType") EventType.of_json
  ; logical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "LogicalResourceId") String.of_json
  ; physical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "PhysicalResourceId") String.of_json
  ; resource_type = Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
  ; timestamp = Aws.Util.option_map (Aws.Json.lookup j "Timestamp") DateTime.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
  ; resource_status =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceStatus") ResourceStatus.of_json
  ; resource_status_reason =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceStatusReason") String.of_json
  ; resource_properties =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceProperties") String.of_json
  ; client_request_token =
      Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
  ; hook_type = Aws.Util.option_map (Aws.Json.lookup j "HookType") String.of_json
  ; hook_status = Aws.Util.option_map (Aws.Json.lookup j "HookStatus") HookStatus.of_json
  ; hook_status_reason =
      Aws.Util.option_map (Aws.Json.lookup j "HookStatusReason") String.of_json
  ; hook_invocation_point =
      Aws.Util.option_map
        (Aws.Json.lookup j "HookInvocationPoint")
        HookInvocationPoint.of_json
  ; hook_failure_mode =
      Aws.Util.option_map (Aws.Json.lookup j "HookFailureMode") HookFailureMode.of_json
  ; detailed_status =
      Aws.Util.option_map (Aws.Json.lookup j "DetailedStatus") DetailedStatus.of_json
  ; validation_failure_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "ValidationFailureMode")
        HookFailureMode.of_json
  ; validation_name =
      Aws.Util.option_map (Aws.Json.lookup j "ValidationName") String.of_json
  ; validation_status =
      Aws.Util.option_map (Aws.Json.lookup j "ValidationStatus") ValidationStatus.of_json
  ; validation_status_reason =
      Aws.Util.option_map (Aws.Json.lookup j "ValidationStatusReason") String.of_json
  ; validation_path =
      Aws.Util.option_map (Aws.Json.lookup j "ValidationPath") String.of_json
  }
