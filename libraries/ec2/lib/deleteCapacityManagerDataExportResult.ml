open Aws.BaseTypes

type t = { capacity_manager_data_export_id : String.t option }

let make ?capacity_manager_data_export_id () = { capacity_manager_data_export_id }

let parse xml =
  Some
    { capacity_manager_data_export_id =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityManagerDataExportId" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_manager_data_export_id (fun f ->
             Aws.Query.Pair ("CapacityManagerDataExportId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_manager_data_export_id (fun f ->
             "capacityManagerDataExportId", String.to_json f)
       ])

let of_json j =
  { capacity_manager_data_export_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityManagerDataExportId") String.of_json
  }
