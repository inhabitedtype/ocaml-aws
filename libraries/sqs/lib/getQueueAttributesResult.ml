type t = { attributes : QueueAttributeMap.t option }

let make ?attributes () = { attributes }

let parse xml =
  Some
    { attributes =
        Aws.Util.option_bind (Aws.Xml.member "Attributes" xml) QueueAttributeMap.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attributes (fun f ->
             Aws.Query.Pair ("Attributes", QueueAttributeMap.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attributes (fun f ->
             "Attributes", QueueAttributeMap.to_json f)
       ])

let of_json j =
  { attributes =
      Aws.Util.option_map (Aws.Json.lookup j "Attributes") QueueAttributeMap.of_json
  }
