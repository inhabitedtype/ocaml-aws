type t =
  { capacity_reservation_preference : CapacityReservationPreference.t option
  ; capacity_reservation_target : CapacityReservationTarget.t option
  }

let make ?capacity_reservation_preference ?capacity_reservation_target () =
  { capacity_reservation_preference; capacity_reservation_target }

let parse xml =
  Some
    { capacity_reservation_preference =
        Aws.Util.option_bind
          (Aws.Xml.member "CapacityReservationPreference" xml)
          CapacityReservationPreference.parse
    ; capacity_reservation_target =
        Aws.Util.option_bind
          (Aws.Xml.member "CapacityReservationTarget" xml)
          CapacityReservationTarget.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_reservation_target (fun f ->
             Aws.Query.Pair
               ("CapacityReservationTarget", CapacityReservationTarget.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_preference (fun f ->
             Aws.Query.Pair
               ("CapacityReservationPreference", CapacityReservationPreference.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_reservation_target (fun f ->
             "CapacityReservationTarget", CapacityReservationTarget.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_preference (fun f ->
             "CapacityReservationPreference", CapacityReservationPreference.to_json f)
       ])

let of_json j =
  { capacity_reservation_preference =
      Aws.Util.option_map
        (Aws.Json.lookup j "CapacityReservationPreference")
        CapacityReservationPreference.of_json
  ; capacity_reservation_target =
      Aws.Util.option_map
        (Aws.Json.lookup j "CapacityReservationTarget")
        CapacityReservationTarget.of_json
  }
