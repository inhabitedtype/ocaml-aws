type t = { usage_strategy : FleetCapacityReservationUsageStrategy.t option }

let make ?usage_strategy () = { usage_strategy }

let parse xml =
  Some
    { usage_strategy =
        Aws.Util.option_bind
          (Aws.Xml.member "UsageStrategy" xml)
          FleetCapacityReservationUsageStrategy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.usage_strategy (fun f ->
             Aws.Query.Pair
               ("UsageStrategy", FleetCapacityReservationUsageStrategy.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.usage_strategy (fun f ->
             "UsageStrategy", FleetCapacityReservationUsageStrategy.to_json f)
       ])

let of_json j =
  { usage_strategy =
      Aws.Util.option_map
        (Aws.Json.lookup j "UsageStrategy")
        FleetCapacityReservationUsageStrategy.of_json
  }
