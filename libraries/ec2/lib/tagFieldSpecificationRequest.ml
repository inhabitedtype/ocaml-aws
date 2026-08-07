type t =
  { resource_type : TaggableResourceType.t option
  ; tag_keys : TagKeyList.t
  }

let make ?resource_type ?(tag_keys = []) () = { resource_type; tag_keys }

let parse xml =
  Some
    { resource_type =
        Aws.Util.option_bind
          (Aws.Xml.member "ResourceType" xml)
          TaggableResourceType.parse
    ; tag_keys =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagKey" xml) TagKeyList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagKey", TagKeyList.to_query v.tag_keys))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", TaggableResourceType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagKey", TagKeyList.to_json v.tag_keys)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "ResourceType", TaggableResourceType.to_json f)
       ])

let of_json j =
  { resource_type =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceType") TaggableResourceType.of_json
  ; tag_keys = TagKeyList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagKey"))
  }
