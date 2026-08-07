open Aws.BaseTypes

type t =
  { pending_maintenance : String.t option
  ; maintenance_auto_applied_after : DateTime.t option
  ; last_maintenance_applied : DateTime.t option
  }

let make ?pending_maintenance ?maintenance_auto_applied_after ?last_maintenance_applied ()
    =
  { pending_maintenance; maintenance_auto_applied_after; last_maintenance_applied }

let parse xml =
  Some
    { pending_maintenance =
        Aws.Util.option_bind (Aws.Xml.member "pendingMaintenance" xml) String.parse
    ; maintenance_auto_applied_after =
        Aws.Util.option_bind
          (Aws.Xml.member "maintenanceAutoAppliedAfter" xml)
          DateTime.parse
    ; last_maintenance_applied =
        Aws.Util.option_bind (Aws.Xml.member "lastMaintenanceApplied" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_maintenance_applied (fun f ->
             Aws.Query.Pair ("LastMaintenanceApplied", DateTime.to_query f))
       ; Aws.Util.option_map v.maintenance_auto_applied_after (fun f ->
             Aws.Query.Pair ("MaintenanceAutoAppliedAfter", DateTime.to_query f))
       ; Aws.Util.option_map v.pending_maintenance (fun f ->
             Aws.Query.Pair ("PendingMaintenance", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_maintenance_applied (fun f ->
             "lastMaintenanceApplied", DateTime.to_json f)
       ; Aws.Util.option_map v.maintenance_auto_applied_after (fun f ->
             "maintenanceAutoAppliedAfter", DateTime.to_json f)
       ; Aws.Util.option_map v.pending_maintenance (fun f ->
             "pendingMaintenance", String.to_json f)
       ])

let of_json j =
  { pending_maintenance =
      Aws.Util.option_map (Aws.Json.lookup j "pendingMaintenance") String.of_json
  ; maintenance_auto_applied_after =
      Aws.Util.option_map
        (Aws.Json.lookup j "maintenanceAutoAppliedAfter")
        DateTime.of_json
  ; last_maintenance_applied =
      Aws.Util.option_map (Aws.Json.lookup j "lastMaintenanceApplied") DateTime.of_json
  }
