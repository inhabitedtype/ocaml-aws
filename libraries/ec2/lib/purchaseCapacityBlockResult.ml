type t =
  { capacity_reservation : CapacityReservation.t option
  ; capacity_blocks : CapacityBlockSet.t
  }

let make ?capacity_reservation ?(capacity_blocks = []) () =
  { capacity_reservation; capacity_blocks }

let parse xml =
  Some
    { capacity_reservation =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityReservation" xml)
          CapacityReservation.parse
    ; capacity_blocks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityBlockSet" xml)
             CapacityBlockSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("CapacityBlockSet", CapacityBlockSet.to_query v.capacity_blocks))
       ; Aws.Util.option_map v.capacity_reservation (fun f ->
             Aws.Query.Pair ("CapacityReservation", CapacityReservation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("capacityBlockSet", CapacityBlockSet.to_json v.capacity_blocks)
       ; Aws.Util.option_map v.capacity_reservation (fun f ->
             "capacityReservation", CapacityReservation.to_json f)
       ])

let of_json j =
  { capacity_reservation =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityReservation")
        CapacityReservation.of_json
  ; capacity_blocks =
      CapacityBlockSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "capacityBlockSet"))
  }
