type t = { capacity_distribution_strategy : CapacityDistributionStrategy.t option }

let make ?capacity_distribution_strategy () = { capacity_distribution_strategy }

let parse xml =
  Some
    { capacity_distribution_strategy =
        Aws.Util.option_bind
          (Aws.Xml.member "CapacityDistributionStrategy" xml)
          CapacityDistributionStrategy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_distribution_strategy (fun f ->
             Aws.Query.Pair
               ("CapacityDistributionStrategy", CapacityDistributionStrategy.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_distribution_strategy (fun f ->
             "CapacityDistributionStrategy", CapacityDistributionStrategy.to_json f)
       ])

let of_json j =
  { capacity_distribution_strategy =
      Aws.Util.option_map
        (Aws.Json.lookup j "CapacityDistributionStrategy")
        CapacityDistributionStrategy.of_json
  }
