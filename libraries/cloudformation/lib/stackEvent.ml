open Aws.BaseTypes

type t =
  { stack_id : String.t
  ; event_id : String.t
  ; stack_name : String.t
  ; operation_id : String.t option
  ; logical_resource_id : String.t option
  ; physical_resource_id : String.t option
  ; resource_type : String.t option
  ; timestamp : DateTime.t
  ; resource_status : ResourceStatus.t option
  ; resource_status_reason : String.t option
  ; resource_properties : String.t option
  ; client_request_token : String.t option
  ; hook_type : String.t option
  ; hook_status : HookStatus.t option
  ; hook_status_reason : String.t option
  ; hook_invocation_point : HookInvocationPoint.t option
  ; hook_invocation_id : String.t option
  ; hook_failure_mode : HookFailureMode.t option
  ; detailed_status : DetailedStatus.t option
  }

let make
    ~stack_id
    ~event_id
    ~stack_name
    ?operation_id
    ?logical_resource_id
    ?physical_resource_id
    ?resource_type
    ~timestamp
    ?resource_status
    ?resource_status_reason
    ?resource_properties
    ?client_request_token
    ?hook_type
    ?hook_status
    ?hook_status_reason
    ?hook_invocation_point
    ?hook_invocation_id
    ?hook_failure_mode
    ?detailed_status
    () =
  { stack_id
  ; event_id
  ; stack_name
  ; operation_id
  ; logical_resource_id
  ; physical_resource_id
  ; resource_type
  ; timestamp
  ; resource_status
  ; resource_status_reason
  ; resource_properties
  ; client_request_token
  ; hook_type
  ; hook_status
  ; hook_status_reason
  ; hook_invocation_point
  ; hook_invocation_id
  ; hook_failure_mode
  ; detailed_status
  }

let parse xml =
  Some
    { stack_id =
        Aws.Xml.required
          "StackId"
          (Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse)
    ; event_id =
        Aws.Xml.required
          "EventId"
          (Aws.Util.option_bind (Aws.Xml.member "EventId" xml) String.parse)
    ; stack_name =
        Aws.Xml.required
          "StackName"
          (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
    ; operation_id = Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
    ; logical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse
    ; physical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "PhysicalResourceId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
    ; timestamp =
        Aws.Xml.required
          "Timestamp"
          (Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse)
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
    ; hook_invocation_id =
        Aws.Util.option_bind (Aws.Xml.member "HookInvocationId" xml) String.parse
    ; hook_failure_mode =
        Aws.Util.option_bind (Aws.Xml.member "HookFailureMode" xml) HookFailureMode.parse
    ; detailed_status =
        Aws.Util.option_bind (Aws.Xml.member "DetailedStatus" xml) DetailedStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.detailed_status (fun f ->
             Aws.Query.Pair ("DetailedStatus", DetailedStatus.to_query f))
       ; Aws.Util.option_map v.hook_failure_mode (fun f ->
             Aws.Query.Pair ("HookFailureMode", HookFailureMode.to_query f))
       ; Aws.Util.option_map v.hook_invocation_id (fun f ->
             Aws.Query.Pair ("HookInvocationId", String.to_query f))
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
       ; Some (Aws.Query.Pair ("Timestamp", DateTime.to_query v.timestamp))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", String.to_query f))
       ; Aws.Util.option_map v.physical_resource_id (fun f ->
             Aws.Query.Pair ("PhysicalResourceId", String.to_query f))
       ; Aws.Util.option_map v.logical_resource_id (fun f ->
             Aws.Query.Pair ("LogicalResourceId", String.to_query f))
       ; Aws.Util.option_map v.operation_id (fun f ->
             Aws.Query.Pair ("OperationId", String.to_query f))
       ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
       ; Some (Aws.Query.Pair ("EventId", String.to_query v.event_id))
       ; Some (Aws.Query.Pair ("StackId", String.to_query v.stack_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.detailed_status (fun f ->
             "DetailedStatus", DetailedStatus.to_json f)
       ; Aws.Util.option_map v.hook_failure_mode (fun f ->
             "HookFailureMode", HookFailureMode.to_json f)
       ; Aws.Util.option_map v.hook_invocation_id (fun f ->
             "HookInvocationId", String.to_json f)
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
       ; Some ("Timestamp", DateTime.to_json v.timestamp)
       ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
       ; Aws.Util.option_map v.physical_resource_id (fun f ->
             "PhysicalResourceId", String.to_json f)
       ; Aws.Util.option_map v.logical_resource_id (fun f ->
             "LogicalResourceId", String.to_json f)
       ; Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
       ; Some ("StackName", String.to_json v.stack_name)
       ; Some ("EventId", String.to_json v.event_id)
       ; Some ("StackId", String.to_json v.stack_id)
       ])

let of_json j =
  { stack_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackId"))
  ; event_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EventId"))
  ; stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
  ; operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
  ; logical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "LogicalResourceId") String.of_json
  ; physical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "PhysicalResourceId") String.of_json
  ; resource_type = Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
  ; timestamp = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamp"))
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
  ; hook_invocation_id =
      Aws.Util.option_map (Aws.Json.lookup j "HookInvocationId") String.of_json
  ; hook_failure_mode =
      Aws.Util.option_map (Aws.Json.lookup j "HookFailureMode") HookFailureMode.of_json
  ; detailed_status =
      Aws.Util.option_map (Aws.Json.lookup j "DetailedStatus") DetailedStatus.of_json
  }
