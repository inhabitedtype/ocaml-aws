open Aws.BaseTypes

type t =
  { traffic_policy_summaries : TrafficPolicySummaries.t
  ; is_truncated : Boolean.t
  ; traffic_policy_id_marker : String.t
  ; max_items : String.t
  }

let make ~traffic_policy_summaries ~is_truncated ~traffic_policy_id_marker ~max_items () =
  { traffic_policy_summaries; is_truncated; traffic_policy_id_marker; max_items }

let parse xml =
  Some
    { traffic_policy_summaries =
        Aws.Xml.required
          "TrafficPolicySummaries"
          (Aws.Util.option_bind
             (Aws.Xml.member "TrafficPolicySummaries" xml)
             TrafficPolicySummaries.parse)
    ; is_truncated =
        Aws.Xml.required
          "IsTruncated"
          (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
    ; traffic_policy_id_marker =
        Aws.Xml.required
          "TrafficPolicyIdMarker"
          (Aws.Util.option_bind (Aws.Xml.member "TrafficPolicyIdMarker" xml) String.parse)
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
              ("TrafficPolicyIdMarker", String.to_query v.traffic_policy_id_marker))
       ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
       ; Some
           (Aws.Query.Pair
              ( "TrafficPolicySummaries.member"
              , TrafficPolicySummaries.to_query v.traffic_policy_summaries ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MaxItems", String.to_json v.max_items)
       ; Some ("TrafficPolicyIdMarker", String.to_json v.traffic_policy_id_marker)
       ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
       ; Some
           ( "TrafficPolicySummaries"
           , TrafficPolicySummaries.to_json v.traffic_policy_summaries )
       ])

let of_json j =
  { traffic_policy_summaries =
      TrafficPolicySummaries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicySummaries"))
  ; is_truncated =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
  ; traffic_policy_id_marker =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyIdMarker"))
  ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
  }
