open Aws.BaseTypes

type t =
  { resource_type : String.t option
  ; logical_resource_id : String.t option
  ; resource_identifier : ResourceIdentifierProperties.t option
  ; resource_status : GeneratedTemplateResourceStatus.t option
  ; resource_status_reason : String.t option
  ; warnings : WarningDetails.t
  }

let make
    ?resource_type
    ?logical_resource_id
    ?resource_identifier
    ?resource_status
    ?resource_status_reason
    ?(warnings = [])
    () =
  { resource_type
  ; logical_resource_id
  ; resource_identifier
  ; resource_status
  ; resource_status_reason
  ; warnings
  }

let parse xml =
  Some
    { resource_type =
        Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
    ; logical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse
    ; resource_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "ResourceIdentifier" xml)
          ResourceIdentifierProperties.parse
    ; resource_status =
        Aws.Util.option_bind
          (Aws.Xml.member "ResourceStatus" xml)
          GeneratedTemplateResourceStatus.parse
    ; resource_status_reason =
        Aws.Util.option_bind (Aws.Xml.member "ResourceStatusReason" xml) String.parse
    ; warnings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Warnings" xml) WarningDetails.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Warnings.member", WarningDetails.to_query v.warnings))
       ; Aws.Util.option_map v.resource_status_reason (fun f ->
             Aws.Query.Pair ("ResourceStatusReason", String.to_query f))
       ; Aws.Util.option_map v.resource_status (fun f ->
             Aws.Query.Pair ("ResourceStatus", GeneratedTemplateResourceStatus.to_query f))
       ; Aws.Util.option_map v.resource_identifier (fun f ->
             Aws.Query.Pair ("ResourceIdentifier", ResourceIdentifierProperties.to_query f))
       ; Aws.Util.option_map v.logical_resource_id (fun f ->
             Aws.Query.Pair ("LogicalResourceId", String.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Warnings", WarningDetails.to_json v.warnings)
       ; Aws.Util.option_map v.resource_status_reason (fun f ->
             "ResourceStatusReason", String.to_json f)
       ; Aws.Util.option_map v.resource_status (fun f ->
             "ResourceStatus", GeneratedTemplateResourceStatus.to_json f)
       ; Aws.Util.option_map v.resource_identifier (fun f ->
             "ResourceIdentifier", ResourceIdentifierProperties.to_json f)
       ; Aws.Util.option_map v.logical_resource_id (fun f ->
             "LogicalResourceId", String.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
       ])

let of_json j =
  { resource_type = Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
  ; logical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "LogicalResourceId") String.of_json
  ; resource_identifier =
      Aws.Util.option_map
        (Aws.Json.lookup j "ResourceIdentifier")
        ResourceIdentifierProperties.of_json
  ; resource_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "ResourceStatus")
        GeneratedTemplateResourceStatus.of_json
  ; resource_status_reason =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceStatusReason") String.of_json
  ; warnings =
      WarningDetails.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Warnings"))
  }
