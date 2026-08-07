type t = { traffic_mirror_session : TrafficMirrorSession.t option }

let make ?traffic_mirror_session () = { traffic_mirror_session }

let parse xml =
  Some
    { traffic_mirror_session =
        Aws.Util.option_bind
          (Aws.Xml.member "trafficMirrorSession" xml)
          TrafficMirrorSession.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.traffic_mirror_session (fun f ->
             Aws.Query.Pair ("TrafficMirrorSession", TrafficMirrorSession.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.traffic_mirror_session (fun f ->
             "trafficMirrorSession", TrafficMirrorSession.to_json f)
       ])

let of_json j =
  { traffic_mirror_session =
      Aws.Util.option_map
        (Aws.Json.lookup j "trafficMirrorSession")
        TrafficMirrorSession.of_json
  }
