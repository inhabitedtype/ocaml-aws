open Aws.BaseTypes

type t =
  { capacity_manager_data_export_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~capacity_manager_data_export_id ?dry_run () =
  { capacity_manager_data_export_id; dry_run }

let parse xml =
  Some
    { capacity_manager_data_export_id =
        Aws.Xml.required
          "CapacityManagerDataExportId"
          (Aws.Util.option_bind
             (Aws.Xml.member "CapacityManagerDataExportId" xml)
             String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CapacityManagerDataExportId"
              , String.to_query v.capacity_manager_data_export_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ( "CapacityManagerDataExportId"
           , String.to_json v.capacity_manager_data_export_id )
       ])

let of_json j =
  { capacity_manager_data_export_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityManagerDataExportId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
