open Aws.BaseTypes

type t =
  { traffic_mirror_target : TrafficMirrorTarget.t option
  ; client_token : String.t option
  }

let make ?traffic_mirror_target ?client_token () = { traffic_mirror_target; client_token }

let parse xml =
  Some
    { traffic_mirror_target =
        Aws.Util.option_bind
          (Aws.Xml.member "trafficMirrorTarget" xml)
          TrafficMirrorTarget.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.traffic_mirror_target (fun f ->
             Aws.Query.Pair ("TrafficMirrorTarget", TrafficMirrorTarget.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Aws.Util.option_map v.traffic_mirror_target (fun f ->
             "trafficMirrorTarget", TrafficMirrorTarget.to_json f)
       ])

let of_json j =
  { traffic_mirror_target =
      Aws.Util.option_map
        (Aws.Json.lookup j "trafficMirrorTarget")
        TrafficMirrorTarget.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  }
