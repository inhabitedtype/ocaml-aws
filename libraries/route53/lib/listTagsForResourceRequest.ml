open Aws.BaseTypes

type t =
  { resource_type : TagResourceType.t
  ; resource_id : String.t
  }

let make ~resource_type ~resource_id () = { resource_type; resource_id }

let parse xml =
  Some
    { resource_type =
        Aws.Xml.required
          "ResourceType"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) TagResourceType.parse)
    ; resource_id =
        Aws.Xml.required
          "ResourceId"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ResourceId", String.to_query v.resource_id))
       ; Some (Aws.Query.Pair ("ResourceType", TagResourceType.to_query v.resource_type))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ResourceId", String.to_json v.resource_id)
       ; Some ("ResourceType", TagResourceType.to_json v.resource_type)
       ])

let of_json j =
  { resource_type =
      TagResourceType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
  ; resource_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceId"))
  }
