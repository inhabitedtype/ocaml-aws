open Aws.BaseTypes

type t =
  { stack_id : String.t
  ; logical_resource_id : String.t
  ; physical_resource_id : String.t option
  ; physical_resource_id_context : PhysicalResourceIdContext.t
  ; resource_type : String.t
  ; property_differences : PropertyDifferences.t
  ; stack_resource_drift_status : StackResourceDriftStatus.t
  ; timestamp : DateTime.t
  }

let make
    ~stack_id
    ~logical_resource_id
    ?physical_resource_id
    ?(physical_resource_id_context = [])
    ~resource_type
    ?(property_differences = [])
    ~stack_resource_drift_status
    ~timestamp
    () =
  { stack_id
  ; logical_resource_id
  ; physical_resource_id
  ; physical_resource_id_context
  ; resource_type
  ; property_differences
  ; stack_resource_drift_status
  ; timestamp
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
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Timestamp", DateTime.to_query v.timestamp))
       ; Some
           (Aws.Query.Pair
              ( "StackResourceDriftStatus"
              , StackResourceDriftStatus.to_query v.stack_resource_drift_status ))
       ; Some
           (Aws.Query.Pair
              ( "PropertyDifferences.member"
              , PropertyDifferences.to_query v.property_differences ))
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
       [ Some ("Timestamp", DateTime.to_json v.timestamp)
       ; Some
           ( "StackResourceDriftStatus"
           , StackResourceDriftStatus.to_json v.stack_resource_drift_status )
       ; Some ("PropertyDifferences", PropertyDifferences.to_json v.property_differences)
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
  ; property_differences =
      PropertyDifferences.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PropertyDifferences"))
  ; stack_resource_drift_status =
      StackResourceDriftStatus.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResourceDriftStatus"))
  ; timestamp = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamp"))
  }
