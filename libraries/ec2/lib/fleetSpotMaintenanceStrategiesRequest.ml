type t = { capacity_rebalance : FleetSpotCapacityRebalanceRequest.t option }

let make ?capacity_rebalance () = { capacity_rebalance }

let parse xml =
  Some
    { capacity_rebalance =
        Aws.Util.option_bind
          (Aws.Xml.member "CapacityRebalance" xml)
          FleetSpotCapacityRebalanceRequest.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_rebalance (fun f ->
             Aws.Query.Pair
               ("CapacityRebalance", FleetSpotCapacityRebalanceRequest.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_rebalance (fun f ->
             "CapacityRebalance", FleetSpotCapacityRebalanceRequest.to_json f)
       ])

let of_json j =
  { capacity_rebalance =
      Aws.Util.option_map
        (Aws.Json.lookup j "CapacityRebalance")
        FleetSpotCapacityRebalanceRequest.of_json
  }
