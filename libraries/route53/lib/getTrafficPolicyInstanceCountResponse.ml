open Aws.BaseTypes

type t = { traffic_policy_instance_count : Integer.t }

let make ~traffic_policy_instance_count () = { traffic_policy_instance_count }

let parse xml =
  Some
    { traffic_policy_instance_count =
        Aws.Xml.required
          "TrafficPolicyInstanceCount"
          (Aws.Util.option_bind
             (Aws.Xml.member "TrafficPolicyInstanceCount" xml)
             Integer.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "TrafficPolicyInstanceCount"
              , Integer.to_query v.traffic_policy_instance_count ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("TrafficPolicyInstanceCount", Integer.to_json v.traffic_policy_instance_count)
       ])

let of_json j =
  { traffic_policy_instance_count =
      Integer.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyInstanceCount"))
  }
