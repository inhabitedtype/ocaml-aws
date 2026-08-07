open Aws.BaseTypes

type t =
  { network_interface_permission_id : String.t
  ; force : Boolean.t option
  ; dry_run : Boolean.t option
  }

let make ~network_interface_permission_id ?force ?dry_run () =
  { network_interface_permission_id; force; dry_run }

let parse xml =
  Some
    { network_interface_permission_id =
        Aws.Xml.required
          "NetworkInterfacePermissionId"
          (Aws.Util.option_bind
             (Aws.Xml.member "NetworkInterfacePermissionId" xml)
             String.parse)
    ; force = Aws.Util.option_bind (Aws.Xml.member "Force" xml) Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.force (fun f ->
             Aws.Query.Pair ("Force", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInterfacePermissionId"
              , String.to_query v.network_interface_permission_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.force (fun f -> "Force", Boolean.to_json f)
       ; Some
           ( "NetworkInterfacePermissionId"
           , String.to_json v.network_interface_permission_id )
       ])

let of_json j =
  { network_interface_permission_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkInterfacePermissionId"))
  ; force = Aws.Util.option_map (Aws.Json.lookup j "Force") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
