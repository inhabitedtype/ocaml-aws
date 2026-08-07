type t = { traffic_policy_instance : TrafficPolicyInstance.t }

let make ~traffic_policy_instance () = { traffic_policy_instance }

let parse xml =
  Some
    { traffic_policy_instance =
        Aws.Xml.required
          "TrafficPolicyInstance"
          (Aws.Util.option_bind
             (Aws.Xml.member "TrafficPolicyInstance" xml)
             TrafficPolicyInstance.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "TrafficPolicyInstance"
              , TrafficPolicyInstance.to_query v.traffic_policy_instance ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "TrafficPolicyInstance"
           , TrafficPolicyInstance.to_json v.traffic_policy_instance )
       ])

let of_json j =
  { traffic_policy_instance =
      TrafficPolicyInstance.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyInstance"))
  }
