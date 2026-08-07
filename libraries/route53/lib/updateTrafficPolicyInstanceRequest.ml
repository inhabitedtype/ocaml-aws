open Aws.BaseTypes

type t =
  { id : String.t
  ; t_t_l : Long.t
  ; traffic_policy_id : String.t
  ; traffic_policy_version : Integer.t
  }

let make ~id ~t_t_l ~traffic_policy_id ~traffic_policy_version () =
  { id; t_t_l; traffic_policy_id; traffic_policy_version }

let parse xml =
  Some
    { id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
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
       ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TrafficPolicyVersion", Integer.to_json v.traffic_policy_version)
       ; Some ("TrafficPolicyId", String.to_json v.traffic_policy_id)
       ; Some ("TTL", Long.to_json v.t_t_l)
       ; Some ("Id", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; t_t_l = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TTL"))
  ; traffic_policy_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyId"))
  ; traffic_policy_version =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyVersion"))
  }
