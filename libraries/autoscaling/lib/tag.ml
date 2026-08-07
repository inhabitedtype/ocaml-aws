open Aws.BaseTypes

type t =
  { resource_id : String.t
  ; resource_type : String.t
  ; key : String.t
  ; value : String.t
  ; propagate_at_launch : Boolean.t
  }

let make ~resource_id ~resource_type ~key ~value ~propagate_at_launch () =
  { resource_id; resource_type; key; value; propagate_at_launch }

let parse xml =
  Some
    { resource_id =
        Aws.Xml.required
          "ResourceId"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse)
    ; resource_type =
        Aws.Xml.required
          "ResourceType"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse)
    ; key =
        Aws.Xml.required
          "Key"
          (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse)
    ; value =
        Aws.Xml.required
          "Value"
          (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
    ; propagate_at_launch =
        Aws.Xml.required
          "PropagateAtLaunch"
          (Aws.Util.option_bind (Aws.Xml.member "PropagateAtLaunch" xml) Boolean.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("PropagateAtLaunch", Boolean.to_query v.propagate_at_launch))
       ; Some (Aws.Query.Pair ("Value", String.to_query v.value))
       ; Some (Aws.Query.Pair ("Key", String.to_query v.key))
       ; Some (Aws.Query.Pair ("ResourceType", String.to_query v.resource_type))
       ; Some (Aws.Query.Pair ("ResourceId", String.to_query v.resource_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PropagateAtLaunch", Boolean.to_json v.propagate_at_launch)
       ; Some ("Value", String.to_json v.value)
       ; Some ("Key", String.to_json v.key)
       ; Some ("ResourceType", String.to_json v.resource_type)
       ; Some ("ResourceId", String.to_json v.resource_id)
       ])

let of_json j =
  { resource_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceId"))
  ; resource_type =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
  ; key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key"))
  ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
  ; propagate_at_launch =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PropagateAtLaunch"))
  }
