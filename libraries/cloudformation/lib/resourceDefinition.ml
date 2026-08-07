open Aws.BaseTypes

type t =
  { resource_type : String.t
  ; logical_resource_id : String.t option
  ; resource_identifier : ResourceIdentifierProperties.t
  }

let make ~resource_type ?logical_resource_id ~resource_identifier () =
  { resource_type; logical_resource_id; resource_identifier }

let parse xml =
  Some
    { resource_type =
        Aws.Xml.required
          "ResourceType"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse)
    ; logical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse
    ; resource_identifier =
        Aws.Xml.required
          "ResourceIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceIdentifier" xml)
             ResourceIdentifierProperties.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ResourceIdentifier"
              , ResourceIdentifierProperties.to_query v.resource_identifier ))
       ; Aws.Util.option_map v.logical_resource_id (fun f ->
             Aws.Query.Pair ("LogicalResourceId", String.to_query f))
       ; Some (Aws.Query.Pair ("ResourceType", String.to_query v.resource_type))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ResourceIdentifier"
           , ResourceIdentifierProperties.to_json v.resource_identifier )
       ; Aws.Util.option_map v.logical_resource_id (fun f ->
             "LogicalResourceId", String.to_json f)
       ; Some ("ResourceType", String.to_json v.resource_type)
       ])

let of_json j =
  { resource_type =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
  ; logical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "LogicalResourceId") String.of_json
  ; resource_identifier =
      ResourceIdentifierProperties.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceIdentifier"))
  }
