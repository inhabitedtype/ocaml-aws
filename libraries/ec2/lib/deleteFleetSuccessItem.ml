open Aws.BaseTypes

type t =
  { current_fleet_state : FleetStateCode.t option
  ; previous_fleet_state : FleetStateCode.t option
  ; fleet_id : String.t option
  }

let make ?current_fleet_state ?previous_fleet_state ?fleet_id () =
  { current_fleet_state; previous_fleet_state; fleet_id }

let parse xml =
  Some
    { current_fleet_state =
        Aws.Util.option_bind (Aws.Xml.member "currentFleetState" xml) FleetStateCode.parse
    ; previous_fleet_state =
        Aws.Util.option_bind
          (Aws.Xml.member "previousFleetState" xml)
          FleetStateCode.parse
    ; fleet_id = Aws.Util.option_bind (Aws.Xml.member "fleetId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.fleet_id (fun f ->
             Aws.Query.Pair ("FleetId", String.to_query f))
       ; Aws.Util.option_map v.previous_fleet_state (fun f ->
             Aws.Query.Pair ("PreviousFleetState", FleetStateCode.to_query f))
       ; Aws.Util.option_map v.current_fleet_state (fun f ->
             Aws.Query.Pair ("CurrentFleetState", FleetStateCode.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.fleet_id (fun f -> "fleetId", String.to_json f)
       ; Aws.Util.option_map v.previous_fleet_state (fun f ->
             "previousFleetState", FleetStateCode.to_json f)
       ; Aws.Util.option_map v.current_fleet_state (fun f ->
             "currentFleetState", FleetStateCode.to_json f)
       ])

let of_json j =
  { current_fleet_state =
      Aws.Util.option_map (Aws.Json.lookup j "currentFleetState") FleetStateCode.of_json
  ; previous_fleet_state =
      Aws.Util.option_map (Aws.Json.lookup j "previousFleetState") FleetStateCode.of_json
  ; fleet_id = Aws.Util.option_map (Aws.Json.lookup j "fleetId") String.of_json
  }
