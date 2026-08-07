type t = { dynamic_routing : DynamicRoutingValue.t option }

let make ?dynamic_routing () = { dynamic_routing }

let parse xml =
  Some
    { dynamic_routing =
        Aws.Util.option_bind
          (Aws.Xml.member "dynamicRouting" xml)
          DynamicRoutingValue.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dynamic_routing (fun f ->
             Aws.Query.Pair ("DynamicRouting", DynamicRoutingValue.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dynamic_routing (fun f ->
             "dynamicRouting", DynamicRoutingValue.to_json f)
       ])

let of_json j =
  { dynamic_routing =
      Aws.Util.option_map (Aws.Json.lookup j "dynamicRouting") DynamicRoutingValue.of_json
  }
