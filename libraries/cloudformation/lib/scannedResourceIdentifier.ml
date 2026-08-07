open Aws.BaseTypes

type t =
  { resource_type : String.t
  ; resource_identifier : JazzResourceIdentifierProperties.t
  }

let make ~resource_type ~resource_identifier () = { resource_type; resource_identifier }

let parse xml =
  Some
    { resource_type =
        Aws.Xml.required
          "ResourceType"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse)
    ; resource_identifier =
        Aws.Xml.required
          "ResourceIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceIdentifier" xml)
             JazzResourceIdentifierProperties.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ResourceIdentifier"
              , JazzResourceIdentifierProperties.to_query v.resource_identifier ))
       ; Some (Aws.Query.Pair ("ResourceType", String.to_query v.resource_type))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ResourceIdentifier"
           , JazzResourceIdentifierProperties.to_json v.resource_identifier )
       ; Some ("ResourceType", String.to_json v.resource_type)
       ])

let of_json j =
  { resource_type =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
  ; resource_identifier =
      JazzResourceIdentifierProperties.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceIdentifier"))
  }
