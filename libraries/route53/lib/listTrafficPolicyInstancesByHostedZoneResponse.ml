open Aws.BaseTypes

type t =
  { traffic_policy_instances : TrafficPolicyInstances.t
  ; traffic_policy_instance_name_marker : String.t option
  ; traffic_policy_instance_type_marker : RRType.t option
  ; is_truncated : Boolean.t
  ; max_items : String.t
  }

let make
    ~traffic_policy_instances
    ?traffic_policy_instance_name_marker
    ?traffic_policy_instance_type_marker
    ~is_truncated
    ~max_items
    () =
  { traffic_policy_instances
  ; traffic_policy_instance_name_marker
  ; traffic_policy_instance_type_marker
  ; is_truncated
  ; max_items
  }

let parse xml =
  Some
    { traffic_policy_instances =
        Aws.Xml.required
          "TrafficPolicyInstances"
          (Aws.Util.option_bind
             (Aws.Xml.member "TrafficPolicyInstances" xml)
             TrafficPolicyInstances.parse)
    ; traffic_policy_instance_name_marker =
        Aws.Util.option_bind
          (Aws.Xml.member "TrafficPolicyInstanceNameMarker" xml)
          String.parse
    ; traffic_policy_instance_type_marker =
        Aws.Util.option_bind
          (Aws.Xml.member "TrafficPolicyInstanceTypeMarker" xml)
          RRType.parse
    ; is_truncated =
        Aws.Xml.required
          "IsTruncated"
          (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
    ; max_items =
        Aws.Xml.required
          "MaxItems"
          (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
       ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
       ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
             Aws.Query.Pair ("TrafficPolicyInstanceTypeMarker", RRType.to_query f))
       ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
             Aws.Query.Pair ("TrafficPolicyInstanceNameMarker", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TrafficPolicyInstances.member"
              , TrafficPolicyInstances.to_query v.traffic_policy_instances ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MaxItems", String.to_json v.max_items)
       ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
       ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
             "TrafficPolicyInstanceTypeMarker", RRType.to_json f)
       ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
             "TrafficPolicyInstanceNameMarker", String.to_json f)
       ; Some
           ( "TrafficPolicyInstances"
           , TrafficPolicyInstances.to_json v.traffic_policy_instances )
       ])

let of_json j =
  { traffic_policy_instances =
      TrafficPolicyInstances.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyInstances"))
  ; traffic_policy_instance_name_marker =
      Aws.Util.option_map
        (Aws.Json.lookup j "TrafficPolicyInstanceNameMarker")
        String.of_json
  ; traffic_policy_instance_type_marker =
      Aws.Util.option_map
        (Aws.Json.lookup j "TrafficPolicyInstanceTypeMarker")
        RRType.of_json
  ; is_truncated =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
  ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
  }
