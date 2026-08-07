open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; spot_fleet_request_config : SpotFleetRequestConfigData.t
  }

let make ?dry_run ~spot_fleet_request_config () = { dry_run; spot_fleet_request_config }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; spot_fleet_request_config =
        Aws.Xml.required
          "spotFleetRequestConfig"
          (Aws.Util.option_bind
             (Aws.Xml.member "spotFleetRequestConfig" xml)
             SpotFleetRequestConfigData.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SpotFleetRequestConfig"
              , SpotFleetRequestConfigData.to_query v.spot_fleet_request_config ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "spotFleetRequestConfig"
           , SpotFleetRequestConfigData.to_json v.spot_fleet_request_config )
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; spot_fleet_request_config =
      SpotFleetRequestConfigData.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "spotFleetRequestConfig"))
  }
