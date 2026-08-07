type t = { bandwidth_weighting : InstanceBandwidthWeighting.t option }

let make ?bandwidth_weighting () = { bandwidth_weighting }

let parse xml =
  Some
    { bandwidth_weighting =
        Aws.Util.option_bind
          (Aws.Xml.member "bandwidthWeighting" xml)
          InstanceBandwidthWeighting.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bandwidth_weighting (fun f ->
             Aws.Query.Pair ("BandwidthWeighting", InstanceBandwidthWeighting.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bandwidth_weighting (fun f ->
             "bandwidthWeighting", InstanceBandwidthWeighting.to_json f)
       ])

let of_json j =
  { bandwidth_weighting =
      Aws.Util.option_map
        (Aws.Json.lookup j "bandwidthWeighting")
        InstanceBandwidthWeighting.of_json
  }
