open Aws.BaseTypes

type t =
  { hosted_zone_id : String.t
  ; name : String.t
  ; t_t_l : Long.t
  ; traffic_policy_id : String.t
  ; traffic_policy_version : Integer.t
  }

let make ~hosted_zone_id ~name ~t_t_l ~traffic_policy_id ~traffic_policy_version () =
  { hosted_zone_id; name; t_t_l; traffic_policy_id; traffic_policy_version }

let parse xml =
  Some
    { hosted_zone_id =
        Aws.Xml.required
          "HostedZoneId"
          (Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse)
    ; name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; t_t_l =
        Aws.Xml.required
          "TTL"
          (Aws.Util.option_bind (Aws.Xml.member "TTL" xml) Long.parse)
    ; traffic_policy_id =
        Aws.Xml.required
          "TrafficPolicyId"
          (Aws.Util.option_bind (Aws.Xml.member "TrafficPolicyId" xml) String.parse)
    ; traffic_policy_version =
        Aws.Xml.required
          "TrafficPolicyVersion"
          (Aws.Util.option_bind (Aws.Xml.member "TrafficPolicyVersion" xml) Integer.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TrafficPolicyVersion", Integer.to_query v.traffic_policy_version))
       ; Some (Aws.Query.Pair ("TrafficPolicyId", String.to_query v.traffic_policy_id))
       ; Some (Aws.Query.Pair ("TTL", Long.to_query v.t_t_l))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ; Some (Aws.Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TrafficPolicyVersion", Integer.to_json v.traffic_policy_version)
       ; Some ("TrafficPolicyId", String.to_json v.traffic_policy_id)
       ; Some ("TTL", Long.to_json v.t_t_l)
       ; Some ("Name", String.to_json v.name)
       ; Some ("HostedZoneId", String.to_json v.hosted_zone_id)
       ])

let of_json j =
  { hosted_zone_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneId"))
  ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; t_t_l = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TTL"))
  ; traffic_policy_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyId"))
  ; traffic_policy_version =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyVersion"))
  }
