type t = { traffic_mirror_filter_rule : TrafficMirrorFilterRule.t option }

let make ?traffic_mirror_filter_rule () = { traffic_mirror_filter_rule }

let parse xml =
  Some
    { traffic_mirror_filter_rule =
        Aws.Util.option_bind
          (Aws.Xml.member "trafficMirrorFilterRule" xml)
          TrafficMirrorFilterRule.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.traffic_mirror_filter_rule (fun f ->
             Aws.Query.Pair ("TrafficMirrorFilterRule", TrafficMirrorFilterRule.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.traffic_mirror_filter_rule (fun f ->
             "trafficMirrorFilterRule", TrafficMirrorFilterRule.to_json f)
       ])

let of_json j =
  { traffic_mirror_filter_rule =
      Aws.Util.option_map
        (Aws.Json.lookup j "trafficMirrorFilterRule")
        TrafficMirrorFilterRule.of_json
  }
