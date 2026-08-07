open Aws.BaseTypes

type t =
  { stack_id : String.t
  ; logical_resource_id : String.t
  ; physical_resource_id : String.t option
  ; physical_resource_id_context : PhysicalResourceIdContext.t
  ; resource_type : String.t
  ; expected_properties : String.t option
  ; actual_properties : String.t option
  ; property_differences : PropertyDifferences.t
  ; stack_resource_drift_status : StackResourceDriftStatus.t
  ; timestamp : DateTime.t
  ; module_info : ModuleInfo.t option
  ; drift_status_reason : String.t option
  }

let make
    ~stack_id
    ~logical_resource_id
    ?physical_resource_id
    ?(physical_resource_id_context = [])
    ~resource_type
    ?expected_properties
    ?actual_properties
    ?(property_differences = [])
    ~stack_resource_drift_status
    ~timestamp
    ?module_info
    ?drift_status_reason
    () =
  { stack_id
  ; logical_resource_id
  ; physical_resource_id
  ; physical_resource_id_context
  ; resource_type
  ; expected_properties
  ; actual_properties
  ; property_differences
  ; stack_resource_drift_status
  ; timestamp
  ; module_info
  ; drift_status_reason
  }

let parse xml =
  Some
    { stack_id =
        Aws.Xml.required
          "StackId"
          (Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse)
    ; logical_resource_id =
        Aws.Xml.required
          "LogicalResourceId"
          (Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse)
    ; physical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "PhysicalResourceId" xml) String.parse
    ; physical_resource_id_context =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PhysicalResourceIdContext" xml)
             PhysicalResourceIdContext.parse)
    ; resource_type =
        Aws.Xml.required
          "ResourceType"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse)
    ; expected_properties =
        Aws.Util.option_bind (Aws.Xml.member "ExpectedProperties" xml) String.parse
    ; actual_properties =
        Aws.Util.option_bind (Aws.Xml.member "ActualProperties" xml) String.parse
    ; property_differences =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PropertyDifferences" xml)
             PropertyDifferences.parse)
    ; stack_resource_drift_status =
        Aws.Xml.required
          "StackResourceDriftStatus"
          (Aws.Util.option_bind
             (Aws.Xml.member "StackResourceDriftStatus" xml)
             StackResourceDriftStatus.parse)
    ; timestamp =
        Aws.Xml.required
          "Timestamp"
          (Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse)
    ; module_info =
        Aws.Util.option_bind (Aws.Xml.member "ModuleInfo" xml) ModuleInfo.parse
    ; drift_status_reason =
        Aws.Util.option_bind (Aws.Xml.member "DriftStatusReason" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.drift_status_reason (fun f ->
             Aws.Query.Pair ("DriftStatusReason", String.to_query f))
       ; Aws.Util.option_map v.module_info (fun f ->
             Aws.Query.Pair ("ModuleInfo", ModuleInfo.to_query f))
       ; Some (Aws.Query.Pair ("Timestamp", DateTime.to_query v.timestamp))
       ; Some
           (Aws.Query.Pair
              ( "StackResourceDriftStatus"
              , StackResourceDriftStatus.to_query v.stack_resource_drift_status ))
       ; Some
           (Aws.Query.Pair
              ( "PropertyDifferences.member"
              , PropertyDifferences.to_query v.property_differences ))
       ; Aws.Util.option_map v.actual_properties (fun f ->
             Aws.Query.Pair ("ActualProperties", String.to_query f))
       ; Aws.Util.option_map v.expected_properties (fun f ->
             Aws.Query.Pair ("ExpectedProperties", String.to_query f))
       ; Some (Aws.Query.Pair ("ResourceType", String.to_query v.resource_type))
       ; Some
           (Aws.Query.Pair
              ( "PhysicalResourceIdContext.member"
              , PhysicalResourceIdContext.to_query v.physical_resource_id_context ))
       ; Aws.Util.option_map v.physical_resource_id (fun f ->
             Aws.Query.Pair ("PhysicalResourceId", String.to_query f))
       ; Some
           (Aws.Query.Pair ("LogicalResourceId", String.to_query v.logical_resource_id))
       ; Some (Aws.Query.Pair ("StackId", String.to_query v.stack_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.drift_status_reason (fun f ->
             "DriftStatusReason", String.to_json f)
       ; Aws.Util.option_map v.module_info (fun f -> "ModuleInfo", ModuleInfo.to_json f)
       ; Some ("Timestamp", DateTime.to_json v.timestamp)
       ; Some
           ( "StackResourceDriftStatus"
           , StackResourceDriftStatus.to_json v.stack_resource_drift_status )
       ; Some ("PropertyDifferences", PropertyDifferences.to_json v.property_differences)
       ; Aws.Util.option_map v.actual_properties (fun f ->
             "ActualProperties", String.to_json f)
       ; Aws.Util.option_map v.expected_properties (fun f ->
             "ExpectedProperties", String.to_json f)
       ; Some ("ResourceType", String.to_json v.resource_type)
       ; Some
           ( "PhysicalResourceIdContext"
           , PhysicalResourceIdContext.to_json v.physical_resource_id_context )
       ; Aws.Util.option_map v.physical_resource_id (fun f ->
             "PhysicalResourceId", String.to_json f)
       ; Some ("LogicalResourceId", String.to_json v.logical_resource_id)
       ; Some ("StackId", String.to_json v.stack_id)
       ])

let of_json j =
  { stack_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackId"))
  ; logical_resource_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceId"))
  ; physical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "PhysicalResourceId") String.of_json
  ; physical_resource_id_context =
      PhysicalResourceIdContext.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PhysicalResourceIdContext"))
  ; resource_type =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
  ; expected_properties =
      Aws.Util.option_map (Aws.Json.lookup j "ExpectedProperties") String.of_json
  ; actual_properties =
      Aws.Util.option_map (Aws.Json.lookup j "ActualProperties") String.of_json
  ; property_differences =
      PropertyDifferences.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PropertyDifferences"))
  ; stack_resource_drift_status =
      StackResourceDriftStatus.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResourceDriftStatus"))
  ; timestamp = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamp"))
  ; module_info = Aws.Util.option_map (Aws.Json.lookup j "ModuleInfo") ModuleInfo.of_json
  ; drift_status_reason =
      Aws.Util.option_map (Aws.Json.lookup j "DriftStatusReason") String.of_json
  }
