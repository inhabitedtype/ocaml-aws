open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; bandwidth_weighting : InstanceBandwidthWeighting.t option
  }

let make ?instance_id ?bandwidth_weighting () = { instance_id; bandwidth_weighting }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; bandwidth_weighting =
        Aws.Util.option_bind
          (Aws.Xml.member "bandwidthWeighting" xml)
          InstanceBandwidthWeighting.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bandwidth_weighting (fun f ->
             Aws.Query.Pair ("BandwidthWeighting", InstanceBandwidthWeighting.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bandwidth_weighting (fun f ->
             "bandwidthWeighting", InstanceBandwidthWeighting.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; bandwidth_weighting =
      Aws.Util.option_map
        (Aws.Json.lookup j "bandwidthWeighting")
        InstanceBandwidthWeighting.of_json
  }
