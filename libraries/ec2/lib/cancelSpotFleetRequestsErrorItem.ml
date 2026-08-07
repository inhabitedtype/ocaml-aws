open Aws.BaseTypes

type t =
  { error : CancelSpotFleetRequestsError.t option
  ; spot_fleet_request_id : String.t option
  }

let make ?error ?spot_fleet_request_id () = { error; spot_fleet_request_id }

let parse xml =
  Some
    { error =
        Aws.Util.option_bind
          (Aws.Xml.member "error" xml)
          CancelSpotFleetRequestsError.parse
    ; spot_fleet_request_id =
        Aws.Util.option_bind (Aws.Xml.member "spotFleetRequestId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.spot_fleet_request_id (fun f ->
             Aws.Query.Pair ("SpotFleetRequestId", String.to_query f))
       ; Aws.Util.option_map v.error (fun f ->
             Aws.Query.Pair ("Error", CancelSpotFleetRequestsError.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.spot_fleet_request_id (fun f ->
             "spotFleetRequestId", String.to_json f)
       ; Aws.Util.option_map v.error (fun f ->
             "error", CancelSpotFleetRequestsError.to_json f)
       ])

let of_json j =
  { error =
      Aws.Util.option_map (Aws.Json.lookup j "error") CancelSpotFleetRequestsError.of_json
  ; spot_fleet_request_id =
      Aws.Util.option_map (Aws.Json.lookup j "spotFleetRequestId") String.of_json
  }
