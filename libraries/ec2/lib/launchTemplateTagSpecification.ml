type t =
  { resource_type : ResourceType.t option
  ; tags : TagList.t
  }

let make ?resource_type ?(tags = []) () = { resource_type; tags }

let parse xml =
  Some
    { resource_type =
        Aws.Util.option_bind (Aws.Xml.member "resourceType" xml) ResourceType.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", ResourceType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "resourceType", ResourceType.to_json f)
       ])

let of_json j =
  { resource_type =
      Aws.Util.option_map (Aws.Json.lookup j "resourceType") ResourceType.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
