type t =
  { capacity_reservation_preference : CapacityReservationPreference.t option
  ; capacity_reservation_target : CapacityReservationTargetResponse.t option
  }

let make ?capacity_reservation_preference ?capacity_reservation_target () =
  { capacity_reservation_preference; capacity_reservation_target }

let parse xml =
  Some
    { capacity_reservation_preference =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityReservationPreference" xml)
          CapacityReservationPreference.parse
    ; capacity_reservation_target =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityReservationTarget" xml)
          CapacityReservationTargetResponse.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_reservation_target (fun f ->
             Aws.Query.Pair
               ("CapacityReservationTarget", CapacityReservationTargetResponse.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_preference (fun f ->
             Aws.Query.Pair
               ("CapacityReservationPreference", CapacityReservationPreference.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_reservation_target (fun f ->
             "capacityReservationTarget", CapacityReservationTargetResponse.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_preference (fun f ->
             "capacityReservationPreference", CapacityReservationPreference.to_json f)
       ])

let of_json j =
  { capacity_reservation_preference =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityReservationPreference")
        CapacityReservationPreference.of_json
  ; capacity_reservation_target =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityReservationTarget")
        CapacityReservationTargetResponse.of_json
  }
