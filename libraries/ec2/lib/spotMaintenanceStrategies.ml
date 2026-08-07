type t = { capacity_rebalance : SpotCapacityRebalance.t option }

let make ?capacity_rebalance () = { capacity_rebalance }

let parse xml =
  Some
    { capacity_rebalance =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityRebalance" xml)
          SpotCapacityRebalance.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_rebalance (fun f ->
             Aws.Query.Pair ("CapacityRebalance", SpotCapacityRebalance.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_rebalance (fun f ->
             "capacityRebalance", SpotCapacityRebalance.to_json f)
       ])

let of_json j =
  { capacity_rebalance =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityRebalance")
        SpotCapacityRebalance.of_json
  }
