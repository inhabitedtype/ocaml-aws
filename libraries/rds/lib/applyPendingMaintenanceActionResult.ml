type t =
  { resource_pending_maintenance_actions : ResourcePendingMaintenanceActions.t option }

let make ?resource_pending_maintenance_actions () =
  { resource_pending_maintenance_actions }

let parse xml =
  Some
    { resource_pending_maintenance_actions =
        Aws.Util.option_bind
          (Aws.Xml.member "ResourcePendingMaintenanceActions" xml)
          ResourcePendingMaintenanceActions.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_pending_maintenance_actions (fun f ->
             Aws.Query.Pair
               ( "ResourcePendingMaintenanceActions"
               , ResourcePendingMaintenanceActions.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_pending_maintenance_actions (fun f ->
             ( "ResourcePendingMaintenanceActions"
             , ResourcePendingMaintenanceActions.to_json f ))
       ])

let of_json j =
  { resource_pending_maintenance_actions =
      Aws.Util.option_map
        (Aws.Json.lookup j "ResourcePendingMaintenanceActions")
        ResourcePendingMaintenanceActions.of_json
  }
