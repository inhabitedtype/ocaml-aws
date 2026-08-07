open Aws.BaseTypes

type t = { spot_fleet_request_id : String.t option }

let make ?spot_fleet_request_id () = { spot_fleet_request_id }

let parse xml =
  Some
    { spot_fleet_request_id =
        Aws.Util.option_bind (Aws.Xml.member "spotFleetRequestId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.spot_fleet_request_id (fun f ->
             Aws.Query.Pair ("SpotFleetRequestId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.spot_fleet_request_id (fun f ->
             "spotFleetRequestId", String.to_json f)
       ])

let of_json j =
  { spot_fleet_request_id =
      Aws.Util.option_map (Aws.Json.lookup j "spotFleetRequestId") String.of_json
  }
