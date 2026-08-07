open Aws.BaseTypes

type t =
  { traffic_policies : TrafficPolicies.t
  ; is_truncated : Boolean.t
  ; traffic_policy_version_marker : String.t
  ; max_items : String.t
  }

let make ~traffic_policies ~is_truncated ~traffic_policy_version_marker ~max_items () =
  { traffic_policies; is_truncated; traffic_policy_version_marker; max_items }

let parse xml =
  Some
    { traffic_policies =
        Aws.Xml.required
          "TrafficPolicies"
          (Aws.Util.option_bind
             (Aws.Xml.member "TrafficPolicies" xml)
             TrafficPolicies.parse)
    ; is_truncated =
        Aws.Xml.required
          "IsTruncated"
          (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
    ; traffic_policy_version_marker =
        Aws.Xml.required
          "TrafficPolicyVersionMarker"
          (Aws.Util.option_bind
             (Aws.Xml.member "TrafficPolicyVersionMarker" xml)
             String.parse)
    ; max_items =
        Aws.Xml.required
          "MaxItems"
          (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
       ; Some
           (Aws.Query.Pair
              ( "TrafficPolicyVersionMarker"
              , String.to_query v.traffic_policy_version_marker ))
       ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
       ; Some
           (Aws.Query.Pair
              ("TrafficPolicies.member", TrafficPolicies.to_query v.traffic_policies))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MaxItems", String.to_json v.max_items)
       ; Some
           ("TrafficPolicyVersionMarker", String.to_json v.traffic_policy_version_marker)
       ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
       ; Some ("TrafficPolicies", TrafficPolicies.to_json v.traffic_policies)
       ])

let of_json j =
  { traffic_policies =
      TrafficPolicies.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicies"))
  ; is_truncated =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
  ; traffic_policy_version_marker =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyVersionMarker"))
  ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
  }
