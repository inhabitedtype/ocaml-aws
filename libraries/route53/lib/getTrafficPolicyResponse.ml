type t = { traffic_policy : TrafficPolicy.t }

let make ~traffic_policy () = { traffic_policy }

let parse xml =
  Some
    { traffic_policy =
        Aws.Xml.required
          "TrafficPolicy"
          (Aws.Util.option_bind (Aws.Xml.member "TrafficPolicy" xml) TrafficPolicy.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TrafficPolicy", TrafficPolicy.to_query v.traffic_policy))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TrafficPolicy", TrafficPolicy.to_json v.traffic_policy) ])

let of_json j =
  { traffic_policy =
      TrafficPolicy.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicy"))
  }
