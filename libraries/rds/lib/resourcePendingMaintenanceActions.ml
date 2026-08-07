open Aws.BaseTypes

type t =
  { resource_identifier : String.t option
  ; pending_maintenance_action_details : PendingMaintenanceActionDetails.t
  }

let make ?resource_identifier ?(pending_maintenance_action_details = []) () =
  { resource_identifier; pending_maintenance_action_details }

let parse xml =
  Some
    { resource_identifier =
        Aws.Util.option_bind (Aws.Xml.member "ResourceIdentifier" xml) String.parse
    ; pending_maintenance_action_details =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PendingMaintenanceActionDetails" xml)
             PendingMaintenanceActionDetails.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "PendingMaintenanceActionDetails.member"
              , PendingMaintenanceActionDetails.to_query
                  v.pending_maintenance_action_details ))
       ; Aws.Util.option_map v.resource_identifier (fun f ->
             Aws.Query.Pair ("ResourceIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "PendingMaintenanceActionDetails"
           , PendingMaintenanceActionDetails.to_json v.pending_maintenance_action_details
           )
       ; Aws.Util.option_map v.resource_identifier (fun f ->
             "ResourceIdentifier", String.to_json f)
       ])

let of_json j =
  { resource_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceIdentifier") String.of_json
  ; pending_maintenance_action_details =
      PendingMaintenanceActionDetails.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PendingMaintenanceActionDetails"))
  }
