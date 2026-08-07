open Aws.BaseTypes

type t =
  { current_fleet_state : CapacityReservationFleetState.t option
  ; previous_fleet_state : CapacityReservationFleetState.t option
  ; capacity_reservation_fleet_id : String.t option
  }

let make ?current_fleet_state ?previous_fleet_state ?capacity_reservation_fleet_id () =
  { current_fleet_state; previous_fleet_state; capacity_reservation_fleet_id }

let parse xml =
  Some
    { current_fleet_state =
        Aws.Util.option_bind
          (Aws.Xml.member "currentFleetState" xml)
          CapacityReservationFleetState.parse
    ; previous_fleet_state =
        Aws.Util.option_bind
          (Aws.Xml.member "previousFleetState" xml)
          CapacityReservationFleetState.parse
    ; capacity_reservation_fleet_id =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityReservationFleetId" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_reservation_fleet_id (fun f ->
             Aws.Query.Pair ("CapacityReservationFleetId", String.to_query f))
       ; Aws.Util.option_map v.previous_fleet_state (fun f ->
             Aws.Query.Pair
               ("PreviousFleetState", CapacityReservationFleetState.to_query f))
       ; Aws.Util.option_map v.current_fleet_state (fun f ->
             Aws.Query.Pair ("CurrentFleetState", CapacityReservationFleetState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_reservation_fleet_id (fun f ->
             "capacityReservationFleetId", String.to_json f)
       ; Aws.Util.option_map v.previous_fleet_state (fun f ->
             "previousFleetState", CapacityReservationFleetState.to_json f)
       ; Aws.Util.option_map v.current_fleet_state (fun f ->
             "currentFleetState", CapacityReservationFleetState.to_json f)
       ])

let of_json j =
  { current_fleet_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "currentFleetState")
        CapacityReservationFleetState.of_json
  ; previous_fleet_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "previousFleetState")
        CapacityReservationFleetState.of_json
  ; capacity_reservation_fleet_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityReservationFleetId") String.of_json
  }
