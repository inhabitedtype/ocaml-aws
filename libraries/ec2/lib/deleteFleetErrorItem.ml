open Aws.BaseTypes

type t =
  { error : DeleteFleetError.t option
  ; fleet_id : String.t option
  }

let make ?error ?fleet_id () = { error; fleet_id }

let parse xml =
  Some
    { error = Aws.Util.option_bind (Aws.Xml.member "error" xml) DeleteFleetError.parse
    ; fleet_id = Aws.Util.option_bind (Aws.Xml.member "fleetId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.fleet_id (fun f ->
             Aws.Query.Pair ("FleetId", String.to_query f))
       ; Aws.Util.option_map v.error (fun f ->
             Aws.Query.Pair ("Error", DeleteFleetError.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.fleet_id (fun f -> "fleetId", String.to_json f)
       ; Aws.Util.option_map v.error (fun f -> "error", DeleteFleetError.to_json f)
       ])

let of_json j =
  { error = Aws.Util.option_map (Aws.Json.lookup j "error") DeleteFleetError.of_json
  ; fleet_id = Aws.Util.option_map (Aws.Json.lookup j "fleetId") String.of_json
  }
