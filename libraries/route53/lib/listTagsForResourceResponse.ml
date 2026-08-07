type t = { resource_tag_set : ResourceTagSet.t }

let make ~resource_tag_set () = { resource_tag_set }

let parse xml =
  Some
    { resource_tag_set =
        Aws.Xml.required
          "ResourceTagSet"
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceTagSet" xml)
             ResourceTagSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("ResourceTagSet", ResourceTagSet.to_query v.resource_tag_set))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ResourceTagSet", ResourceTagSet.to_json v.resource_tag_set) ])

let of_json j =
  { resource_tag_set =
      ResourceTagSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTagSet"))
  }
