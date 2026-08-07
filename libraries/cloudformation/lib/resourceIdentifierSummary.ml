open Aws.BaseTypes

type t =
  { resource_type : String.t option
  ; logical_resource_ids : LogicalResourceIds.t
  ; resource_identifiers : ResourceIdentifiers.t
  }

let make ?resource_type ?(logical_resource_ids = []) ?(resource_identifiers = []) () =
  { resource_type; logical_resource_ids; resource_identifiers }

let parse xml =
  Some
    { resource_type =
        Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
    ; logical_resource_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LogicalResourceIds" xml)
             LogicalResourceIds.parse)
    ; resource_identifiers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceIdentifiers" xml)
             ResourceIdentifiers.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ResourceIdentifiers.member"
              , ResourceIdentifiers.to_query v.resource_identifiers ))
       ; Some
           (Aws.Query.Pair
              ( "LogicalResourceIds.member"
              , LogicalResourceIds.to_query v.logical_resource_ids ))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ResourceIdentifiers", ResourceIdentifiers.to_json v.resource_identifiers)
       ; Some ("LogicalResourceIds", LogicalResourceIds.to_json v.logical_resource_ids)
       ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
       ])

let of_json j =
  { resource_type = Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
  ; logical_resource_ids =
      LogicalResourceIds.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceIds"))
  ; resource_identifiers =
      ResourceIdentifiers.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceIdentifiers"))
  }
