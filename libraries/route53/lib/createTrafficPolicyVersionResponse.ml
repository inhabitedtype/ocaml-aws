open Aws.BaseTypes

type t =
  { traffic_policy : TrafficPolicy.t
  ; location : String.t
  }

let make ~traffic_policy ~location () = { traffic_policy; location }

let parse xml =
  Some
    { traffic_policy =
        Aws.Xml.required
          "TrafficPolicy"
          (Aws.Util.option_bind (Aws.Xml.member "TrafficPolicy" xml) TrafficPolicy.parse)
    ; location =
        Aws.Xml.required
          "Location"
          (Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Location", String.to_query v.location))
       ; Some (Aws.Query.Pair ("TrafficPolicy", TrafficPolicy.to_query v.traffic_policy))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Location", String.to_json v.location)
       ; Some ("TrafficPolicy", TrafficPolicy.to_json v.traffic_policy)
       ])

let of_json j =
  { traffic_policy =
      TrafficPolicy.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicy"))
  ; location = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Location"))
  }
