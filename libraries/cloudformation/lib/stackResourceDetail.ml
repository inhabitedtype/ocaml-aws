open Aws.BaseTypes

type t =
  { stack_name : String.t option
  ; stack_id : String.t option
  ; logical_resource_id : String.t
  ; physical_resource_id : String.t option
  ; resource_type : String.t
  ; last_updated_timestamp : DateTime.t
  ; resource_status : ResourceStatus.t
  ; resource_status_reason : String.t option
  ; description : String.t option
  ; metadata : String.t option
  ; drift_information : StackResourceDriftInformation.t option
  ; module_info : ModuleInfo.t option
  }

let make
    ?stack_name
    ?stack_id
    ~logical_resource_id
    ?physical_resource_id
    ~resource_type
    ~last_updated_timestamp
    ~resource_status
    ?resource_status_reason
    ?description
    ?metadata
    ?drift_information
    ?module_info
    () =
  { stack_name
  ; stack_id
  ; logical_resource_id
  ; physical_resource_id
  ; resource_type
  ; last_updated_timestamp
  ; resource_status
  ; resource_status_reason
  ; description
  ; metadata
  ; drift_information
  ; module_info
  }

let parse xml =
  Some
    { stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
    ; stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
    ; logical_resource_id =
        Aws.Xml.required
          "LogicalResourceId"
          (Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse)
    ; physical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "PhysicalResourceId" xml) String.parse
    ; resource_type =
        Aws.Xml.required
          "ResourceType"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse)
    ; last_updated_timestamp =
        Aws.Xml.required
          "LastUpdatedTimestamp"
          (Aws.Util.option_bind
             (Aws.Xml.member "LastUpdatedTimestamp" xml)
             DateTime.parse)
    ; resource_status =
        Aws.Xml.required
          "ResourceStatus"
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceStatus" xml)
             ResourceStatus.parse)
    ; resource_status_reason =
        Aws.Util.option_bind (Aws.Xml.member "ResourceStatusReason" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; metadata = Aws.Util.option_bind (Aws.Xml.member "Metadata" xml) String.parse
    ; drift_information =
        Aws.Util.option_bind
          (Aws.Xml.member "DriftInformation" xml)
          StackResourceDriftInformation.parse
    ; module_info =
        Aws.Util.option_bind (Aws.Xml.member "ModuleInfo" xml) ModuleInfo.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.module_info (fun f ->
             Aws.Query.Pair ("ModuleInfo", ModuleInfo.to_query f))
       ; Aws.Util.option_map v.drift_information (fun f ->
             Aws.Query.Pair ("DriftInformation", StackResourceDriftInformation.to_query f))
       ; Aws.Util.option_map v.metadata (fun f ->
             Aws.Query.Pair ("Metadata", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.resource_status_reason (fun f ->
             Aws.Query.Pair ("ResourceStatusReason", String.to_query f))
       ; Some
           (Aws.Query.Pair ("ResourceStatus", ResourceStatus.to_query v.resource_status))
       ; Some
           (Aws.Query.Pair
              ("LastUpdatedTimestamp", DateTime.to_query v.last_updated_timestamp))
       ; Some (Aws.Query.Pair ("ResourceType", String.to_query v.resource_type))
       ; Aws.Util.option_map v.physical_resource_id (fun f ->
             Aws.Query.Pair ("PhysicalResourceId", String.to_query f))
       ; Some
           (Aws.Query.Pair ("LogicalResourceId", String.to_query v.logical_resource_id))
       ; Aws.Util.option_map v.stack_id (fun f ->
             Aws.Query.Pair ("StackId", String.to_query f))
       ; Aws.Util.option_map v.stack_name (fun f ->
             Aws.Query.Pair ("StackName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.module_info (fun f -> "ModuleInfo", ModuleInfo.to_json f)
       ; Aws.Util.option_map v.drift_information (fun f ->
             "DriftInformation", StackResourceDriftInformation.to_json f)
       ; Aws.Util.option_map v.metadata (fun f -> "Metadata", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.resource_status_reason (fun f ->
             "ResourceStatusReason", String.to_json f)
       ; Some ("ResourceStatus", ResourceStatus.to_json v.resource_status)
       ; Some ("LastUpdatedTimestamp", DateTime.to_json v.last_updated_timestamp)
       ; Some ("ResourceType", String.to_json v.resource_type)
       ; Aws.Util.option_map v.physical_resource_id (fun f ->
             "PhysicalResourceId", String.to_json f)
       ; Some ("LogicalResourceId", String.to_json v.logical_resource_id)
       ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
       ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
       ])

let of_json j =
  { stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
  ; stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
  ; logical_resource_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceId"))
  ; physical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "PhysicalResourceId") String.of_json
  ; resource_type =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
  ; last_updated_timestamp =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LastUpdatedTimestamp"))
  ; resource_status =
      ResourceStatus.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceStatus"))
  ; resource_status_reason =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceStatusReason") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; metadata = Aws.Util.option_map (Aws.Json.lookup j "Metadata") String.of_json
  ; drift_information =
      Aws.Util.option_map
        (Aws.Json.lookup j "DriftInformation")
        StackResourceDriftInformation.of_json
  ; module_info = Aws.Util.option_map (Aws.Json.lookup j "ModuleInfo") ModuleInfo.of_json
  }
