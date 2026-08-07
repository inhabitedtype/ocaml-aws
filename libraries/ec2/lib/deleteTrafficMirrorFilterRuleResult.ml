open Aws.BaseTypes

type t = { traffic_mirror_filter_rule_id : String.t option }

let make ?traffic_mirror_filter_rule_id () = { traffic_mirror_filter_rule_id }

let parse xml =
  Some
    { traffic_mirror_filter_rule_id =
        Aws.Util.option_bind (Aws.Xml.member "trafficMirrorFilterRuleId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.traffic_mirror_filter_rule_id (fun f ->
             Aws.Query.Pair ("TrafficMirrorFilterRuleId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.traffic_mirror_filter_rule_id (fun f ->
             "trafficMirrorFilterRuleId", String.to_json f)
       ])

let of_json j =
  { traffic_mirror_filter_rule_id =
      Aws.Util.option_map (Aws.Json.lookup j "trafficMirrorFilterRuleId") String.of_json
  }
