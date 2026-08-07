type t =
  { resource_type : TagResourceType.t
  ; resource_ids : TagResourceIdList.t
  }

let make ~resource_type ~resource_ids () = { resource_type; resource_ids }

let parse xml =
  Some
    { resource_type =
        Aws.Xml.required
          "ResourceType"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) TagResourceType.parse)
    ; resource_ids =
        Aws.Xml.required
          "ResourceIds"
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceIds" xml)
             TagResourceIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ResourceIds.member", TagResourceIdList.to_query v.resource_ids))
       ; Some (Aws.Query.Pair ("ResourceType", TagResourceType.to_query v.resource_type))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ResourceIds", TagResourceIdList.to_json v.resource_ids)
       ; Some ("ResourceType", TagResourceType.to_json v.resource_type)
       ])

let of_json j =
  { resource_type =
      TagResourceType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
  ; resource_ids =
      TagResourceIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceIds"))
  }
