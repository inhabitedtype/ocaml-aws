open Aws.BaseTypes

type t =
  { traffic_mirror_targets : TrafficMirrorTargetSet.t
  ; next_token : String.t option
  }

let make ?(traffic_mirror_targets = []) ?next_token () =
  { traffic_mirror_targets; next_token }

let parse xml =
  Some
    { traffic_mirror_targets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "trafficMirrorTargetSet" xml)
             TrafficMirrorTargetSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TrafficMirrorTargetSet"
              , TrafficMirrorTargetSet.to_query v.traffic_mirror_targets ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "trafficMirrorTargetSet"
           , TrafficMirrorTargetSet.to_json v.traffic_mirror_targets )
       ])

let of_json j =
  { traffic_mirror_targets =
      TrafficMirrorTargetSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "trafficMirrorTargetSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
