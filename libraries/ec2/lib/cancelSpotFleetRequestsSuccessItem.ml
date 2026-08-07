open Aws.BaseTypes

type t =
  { current_spot_fleet_request_state : BatchState.t option
  ; previous_spot_fleet_request_state : BatchState.t option
  ; spot_fleet_request_id : String.t option
  }

let make
    ?current_spot_fleet_request_state
    ?previous_spot_fleet_request_state
    ?spot_fleet_request_id
    () =
  { current_spot_fleet_request_state
  ; previous_spot_fleet_request_state
  ; spot_fleet_request_id
  }

let parse xml =
  Some
    { current_spot_fleet_request_state =
        Aws.Util.option_bind
          (Aws.Xml.member "currentSpotFleetRequestState" xml)
          BatchState.parse
    ; previous_spot_fleet_request_state =
        Aws.Util.option_bind
          (Aws.Xml.member "previousSpotFleetRequestState" xml)
          BatchState.parse
    ; spot_fleet_request_id =
        Aws.Util.option_bind (Aws.Xml.member "spotFleetRequestId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.spot_fleet_request_id (fun f ->
             Aws.Query.Pair ("SpotFleetRequestId", String.to_query f))
       ; Aws.Util.option_map v.previous_spot_fleet_request_state (fun f ->
             Aws.Query.Pair ("PreviousSpotFleetRequestState", BatchState.to_query f))
       ; Aws.Util.option_map v.current_spot_fleet_request_state (fun f ->
             Aws.Query.Pair ("CurrentSpotFleetRequestState", BatchState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.spot_fleet_request_id (fun f ->
             "spotFleetRequestId", String.to_json f)
       ; Aws.Util.option_map v.previous_spot_fleet_request_state (fun f ->
             "previousSpotFleetRequestState", BatchState.to_json f)
       ; Aws.Util.option_map v.current_spot_fleet_request_state (fun f ->
             "currentSpotFleetRequestState", BatchState.to_json f)
       ])

let of_json j =
  { current_spot_fleet_request_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "currentSpotFleetRequestState")
        BatchState.of_json
  ; previous_spot_fleet_request_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "previousSpotFleetRequestState")
        BatchState.of_json
  ; spot_fleet_request_id =
      Aws.Util.option_map (Aws.Json.lookup j "spotFleetRequestId") String.of_json
  }
