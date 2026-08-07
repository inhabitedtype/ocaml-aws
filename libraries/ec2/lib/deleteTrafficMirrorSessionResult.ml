open Aws.BaseTypes

type t = { traffic_mirror_session_id : String.t option }

let make ?traffic_mirror_session_id () = { traffic_mirror_session_id }

let parse xml =
  Some
    { traffic_mirror_session_id =
        Aws.Util.option_bind (Aws.Xml.member "trafficMirrorSessionId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.traffic_mirror_session_id (fun f ->
             Aws.Query.Pair ("TrafficMirrorSessionId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.traffic_mirror_session_id (fun f ->
             "trafficMirrorSessionId", String.to_json f)
       ])

let of_json j =
  { traffic_mirror_session_id =
      Aws.Util.option_map (Aws.Json.lookup j "trafficMirrorSessionId") String.of_json
  }
