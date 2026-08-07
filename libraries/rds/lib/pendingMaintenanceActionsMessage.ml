open Aws.BaseTypes

type t =
  { pending_maintenance_actions : PendingMaintenanceActions.t
  ; marker : String.t option
  }

let make ?(pending_maintenance_actions = []) ?marker () =
  { pending_maintenance_actions; marker }

let parse xml =
  Some
    { pending_maintenance_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PendingMaintenanceActions" xml)
             PendingMaintenanceActions.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PendingMaintenanceActions.member"
              , PendingMaintenanceActions.to_query v.pending_maintenance_actions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some
           ( "PendingMaintenanceActions"
           , PendingMaintenanceActions.to_json v.pending_maintenance_actions )
       ])

let of_json j =
  { pending_maintenance_actions =
      PendingMaintenanceActions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PendingMaintenanceActions"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
