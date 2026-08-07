open Aws.BaseTypes

type t =
  { capacity_reservation_fleet_id : String.t option
  ; cancel_capacity_reservation_fleet_error : CancelCapacityReservationFleetError.t option
  }

let make ?capacity_reservation_fleet_id ?cancel_capacity_reservation_fleet_error () =
  { capacity_reservation_fleet_id; cancel_capacity_reservation_fleet_error }

let parse xml =
  Some
    { capacity_reservation_fleet_id =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityReservationFleetId" xml)
          String.parse
    ; cancel_capacity_reservation_fleet_error =
        Aws.Util.option_bind
          (Aws.Xml.member "cancelCapacityReservationFleetError" xml)
          CancelCapacityReservationFleetError.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cancel_capacity_reservation_fleet_error (fun f ->
             Aws.Query.Pair
               ( "CancelCapacityReservationFleetError"
               , CancelCapacityReservationFleetError.to_query f ))
       ; Aws.Util.option_map v.capacity_reservation_fleet_id (fun f ->
             Aws.Query.Pair ("CapacityReservationFleetId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cancel_capacity_reservation_fleet_error (fun f ->
             ( "cancelCapacityReservationFleetError"
             , CancelCapacityReservationFleetError.to_json f ))
       ; Aws.Util.option_map v.capacity_reservation_fleet_id (fun f ->
             "capacityReservationFleetId", String.to_json f)
       ])

let of_json j =
  { capacity_reservation_fleet_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityReservationFleetId") String.of_json
  ; cancel_capacity_reservation_fleet_error =
      Aws.Util.option_map
        (Aws.Json.lookup j "cancelCapacityReservationFleetError")
        CancelCapacityReservationFleetError.of_json
  }
