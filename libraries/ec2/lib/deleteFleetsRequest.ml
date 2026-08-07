open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; fleet_ids : FleetIdSet.t
  ; terminate_instances : Boolean.t
  }

let make ?dry_run ~fleet_ids ~terminate_instances () =
  { dry_run; fleet_ids; terminate_instances }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; fleet_ids =
        Aws.Xml.required
          "FleetId"
          (Aws.Util.option_bind (Aws.Xml.member "FleetId" xml) FleetIdSet.parse)
    ; terminate_instances =
        Aws.Xml.required
          "TerminateInstances"
          (Aws.Util.option_bind (Aws.Xml.member "TerminateInstances" xml) Boolean.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("TerminateInstances", Boolean.to_query v.terminate_instances))
       ; Some (Aws.Query.Pair ("FleetId", FleetIdSet.to_query v.fleet_ids))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TerminateInstances", Boolean.to_json v.terminate_instances)
       ; Some ("FleetId", FleetIdSet.to_json v.fleet_ids)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; fleet_ids = FleetIdSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "FleetId"))
  ; terminate_instances =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TerminateInstances"))
  }
