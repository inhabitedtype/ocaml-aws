open Aws.BaseTypes

type t =
  { traffic_mirror_filters : TrafficMirrorFilterSet.t
  ; next_token : String.t option
  }

let make ?(traffic_mirror_filters = []) ?next_token () =
  { traffic_mirror_filters; next_token }

let parse xml =
  Some
    { traffic_mirror_filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "trafficMirrorFilterSet" xml)
             TrafficMirrorFilterSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TrafficMirrorFilterSet"
              , TrafficMirrorFilterSet.to_query v.traffic_mirror_filters ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "trafficMirrorFilterSet"
           , TrafficMirrorFilterSet.to_json v.traffic_mirror_filters )
       ])

let of_json j =
  { traffic_mirror_filters =
      TrafficMirrorFilterSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "trafficMirrorFilterSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
