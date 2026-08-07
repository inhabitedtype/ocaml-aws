type t = { tags : TagMap.t option }

let make ?tags () = { tags }

let parse xml =
  Some { tags = Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagMap.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.tags (fun f -> Aws.Query.Pair ("Tags", TagMap.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.tags (fun f -> "Tags", TagMap.to_json f) ])

let of_json j = { tags = Aws.Util.option_map (Aws.Json.lookup j "Tags") TagMap.of_json }
