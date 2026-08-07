open Aws.BaseTypes

type t =
  { action : StackRefactorActionType.t option
  ; entity : StackRefactorActionEntity.t option
  ; physical_resource_id : String.t option
  ; resource_identifier : String.t option
  ; description : String.t option
  ; detection : StackRefactorDetection.t option
  ; detection_reason : String.t option
  ; tag_resources : StackRefactorTagResources.t
  ; untag_resources : StackRefactorUntagResources.t
  ; resource_mapping : ResourceMapping.t option
  }

let make
    ?action
    ?entity
    ?physical_resource_id
    ?resource_identifier
    ?description
    ?detection
    ?detection_reason
    ?(tag_resources = [])
    ?(untag_resources = [])
    ?resource_mapping
    () =
  { action
  ; entity
  ; physical_resource_id
  ; resource_identifier
  ; description
  ; detection
  ; detection_reason
  ; tag_resources
  ; untag_resources
  ; resource_mapping
  }

let parse xml =
  Some
    { action =
        Aws.Util.option_bind (Aws.Xml.member "Action" xml) StackRefactorActionType.parse
    ; entity =
        Aws.Util.option_bind (Aws.Xml.member "Entity" xml) StackRefactorActionEntity.parse
    ; physical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "PhysicalResourceId" xml) String.parse
    ; resource_identifier =
        Aws.Util.option_bind (Aws.Xml.member "ResourceIdentifier" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; detection =
        Aws.Util.option_bind (Aws.Xml.member "Detection" xml) StackRefactorDetection.parse
    ; detection_reason =
        Aws.Util.option_bind (Aws.Xml.member "DetectionReason" xml) String.parse
    ; tag_resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagResources" xml)
             StackRefactorTagResources.parse)
    ; untag_resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UntagResources" xml)
             StackRefactorUntagResources.parse)
    ; resource_mapping =
        Aws.Util.option_bind (Aws.Xml.member "ResourceMapping" xml) ResourceMapping.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_mapping (fun f ->
             Aws.Query.Pair ("ResourceMapping", ResourceMapping.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "UntagResources.member"
              , StackRefactorUntagResources.to_query v.untag_resources ))
       ; Some
           (Aws.Query.Pair
              ("TagResources.member", StackRefactorTagResources.to_query v.tag_resources))
       ; Aws.Util.option_map v.detection_reason (fun f ->
             Aws.Query.Pair ("DetectionReason", String.to_query f))
       ; Aws.Util.option_map v.detection (fun f ->
             Aws.Query.Pair ("Detection", StackRefactorDetection.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.resource_identifier (fun f ->
             Aws.Query.Pair ("ResourceIdentifier", String.to_query f))
       ; Aws.Util.option_map v.physical_resource_id (fun f ->
             Aws.Query.Pair ("PhysicalResourceId", String.to_query f))
       ; Aws.Util.option_map v.entity (fun f ->
             Aws.Query.Pair ("Entity", StackRefactorActionEntity.to_query f))
       ; Aws.Util.option_map v.action (fun f ->
             Aws.Query.Pair ("Action", StackRefactorActionType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_mapping (fun f ->
             "ResourceMapping", ResourceMapping.to_json f)
       ; Some ("UntagResources", StackRefactorUntagResources.to_json v.untag_resources)
       ; Some ("TagResources", StackRefactorTagResources.to_json v.tag_resources)
       ; Aws.Util.option_map v.detection_reason (fun f ->
             "DetectionReason", String.to_json f)
       ; Aws.Util.option_map v.detection (fun f ->
             "Detection", StackRefactorDetection.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.resource_identifier (fun f ->
             "ResourceIdentifier", String.to_json f)
       ; Aws.Util.option_map v.physical_resource_id (fun f ->
             "PhysicalResourceId", String.to_json f)
       ; Aws.Util.option_map v.entity (fun f ->
             "Entity", StackRefactorActionEntity.to_json f)
       ; Aws.Util.option_map v.action (fun f ->
             "Action", StackRefactorActionType.to_json f)
       ])

let of_json j =
  { action =
      Aws.Util.option_map (Aws.Json.lookup j "Action") StackRefactorActionType.of_json
  ; entity =
      Aws.Util.option_map (Aws.Json.lookup j "Entity") StackRefactorActionEntity.of_json
  ; physical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "PhysicalResourceId") String.of_json
  ; resource_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceIdentifier") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; detection =
      Aws.Util.option_map (Aws.Json.lookup j "Detection") StackRefactorDetection.of_json
  ; detection_reason =
      Aws.Util.option_map (Aws.Json.lookup j "DetectionReason") String.of_json
  ; tag_resources =
      StackRefactorTagResources.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagResources"))
  ; untag_resources =
      StackRefactorUntagResources.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "UntagResources"))
  ; resource_mapping =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceMapping") ResourceMapping.of_json
  }
