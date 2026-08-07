type t = { resource_tag_sets : ResourceTagSetList.t }

let make ~resource_tag_sets () = { resource_tag_sets }

let parse xml =
  Some
    { resource_tag_sets =
        Aws.Xml.required
          "ResourceTagSets"
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceTagSets" xml)
             ResourceTagSetList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ResourceTagSets.member", ResourceTagSetList.to_query v.resource_tag_sets))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ResourceTagSets", ResourceTagSetList.to_json v.resource_tag_sets) ])

let of_json j =
  { resource_tag_sets =
      ResourceTagSetList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTagSets"))
  }
