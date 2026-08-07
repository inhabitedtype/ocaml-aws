type t = { capacity_reservation : CapacityReservation.t option }

let make ?capacity_reservation () = { capacity_reservation }

let parse xml =
  Some
    { capacity_reservation =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityReservation" xml)
          CapacityReservation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_reservation (fun f ->
             Aws.Query.Pair ("CapacityReservation", CapacityReservation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_reservation (fun f ->
             "capacityReservation", CapacityReservation.to_json f)
       ])

let of_json j =
  { capacity_reservation =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityReservation")
        CapacityReservation.of_json
  }
