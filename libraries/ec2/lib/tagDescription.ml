open Aws.BaseTypes

type t =
  { key : String.t
  ; resource_id : String.t
  ; resource_type : ResourceType.t
  ; value : String.t
  }

let make ~key ~resource_id ~resource_type ~value () =
  { key; resource_id; resource_type; value }

let parse xml =
  Some
    { key =
        Aws.Xml.required
          "key"
          (Aws.Util.option_bind (Aws.Xml.member "key" xml) String.parse)
    ; resource_id =
        Aws.Xml.required
          "resourceId"
          (Aws.Util.option_bind (Aws.Xml.member "resourceId" xml) String.parse)
    ; resource_type =
        Aws.Xml.required
          "resourceType"
          (Aws.Util.option_bind (Aws.Xml.member "resourceType" xml) ResourceType.parse)
    ; value =
        Aws.Xml.required
          "value"
          (Aws.Util.option_bind (Aws.Xml.member "value" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Value", String.to_query v.value))
       ; Some (Aws.Query.Pair ("ResourceType", ResourceType.to_query v.resource_type))
       ; Some (Aws.Query.Pair ("ResourceId", String.to_query v.resource_id))
       ; Some (Aws.Query.Pair ("Key", String.to_query v.key))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("value", String.to_json v.value)
       ; Some ("resourceType", ResourceType.to_json v.resource_type)
       ; Some ("resourceId", String.to_json v.resource_id)
       ; Some ("key", String.to_json v.key)
       ])

let of_json j =
  { key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "key"))
  ; resource_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "resourceId"))
  ; resource_type =
      ResourceType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "resourceType"))
  ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "value"))
  }
