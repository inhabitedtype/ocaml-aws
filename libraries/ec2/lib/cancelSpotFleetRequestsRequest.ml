open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; spot_fleet_request_ids : SpotFleetRequestIdList.t
  ; terminate_instances : Boolean.t
  }

let make ?dry_run ~spot_fleet_request_ids ~terminate_instances () =
  { dry_run; spot_fleet_request_ids; terminate_instances }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; spot_fleet_request_ids =
        Aws.Xml.required
          "spotFleetRequestId"
          (Aws.Util.option_bind
             (Aws.Xml.member "spotFleetRequestId" xml)
             SpotFleetRequestIdList.parse)
    ; terminate_instances =
        Aws.Xml.required
          "terminateInstances"
          (Aws.Util.option_bind (Aws.Xml.member "terminateInstances" xml) Boolean.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("TerminateInstances", Boolean.to_query v.terminate_instances))
       ; Some
           (Aws.Query.Pair
              ( "SpotFleetRequestId"
              , SpotFleetRequestIdList.to_query v.spot_fleet_request_ids ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("terminateInstances", Boolean.to_json v.terminate_instances)
       ; Some
           ("spotFleetRequestId", SpotFleetRequestIdList.to_json v.spot_fleet_request_ids)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; spot_fleet_request_ids =
      SpotFleetRequestIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "spotFleetRequestId"))
  ; terminate_instances =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "terminateInstances"))
  }
