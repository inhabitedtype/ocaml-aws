type t =
  { capacity_reservation_ids : CapacityReservationIds.t
  ; capacity_reservation_resource_group_arns : CapacityReservationResourceGroupArns.t
  }

let make
    ?(capacity_reservation_ids = [])
    ?(capacity_reservation_resource_group_arns = [])
    () =
  { capacity_reservation_ids; capacity_reservation_resource_group_arns }

let parse xml =
  Some
    { capacity_reservation_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CapacityReservationIds" xml)
             CapacityReservationIds.parse)
    ; capacity_reservation_resource_group_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CapacityReservationResourceGroupArns" xml)
             CapacityReservationResourceGroupArns.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CapacityReservationResourceGroupArns.member"
              , CapacityReservationResourceGroupArns.to_query
                  v.capacity_reservation_resource_group_arns ))
       ; Some
           (Aws.Query.Pair
              ( "CapacityReservationIds.member"
              , CapacityReservationIds.to_query v.capacity_reservation_ids ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "CapacityReservationResourceGroupArns"
           , CapacityReservationResourceGroupArns.to_json
               v.capacity_reservation_resource_group_arns )
       ; Some
           ( "CapacityReservationIds"
           , CapacityReservationIds.to_json v.capacity_reservation_ids )
       ])

let of_json j =
  { capacity_reservation_ids =
      CapacityReservationIds.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityReservationIds"))
  ; capacity_reservation_resource_group_arns =
      CapacityReservationResourceGroupArns.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "CapacityReservationResourceGroupArns"))
  }
