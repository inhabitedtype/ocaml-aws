open Aws.BaseTypes

type t =
  { capacity_manager_data_export_ids : CapacityManagerDataExportIdSet.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; dry_run : Boolean.t option
  ; filters : FilterList.t
  }

let make
    ?(capacity_manager_data_export_ids = [])
    ?max_results
    ?next_token
    ?dry_run
    ?(filters = [])
    () =
  { capacity_manager_data_export_ids; max_results; next_token; dry_run; filters }

let parse xml =
  Some
    { capacity_manager_data_export_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CapacityManagerDataExportId" xml)
             CapacityManagerDataExportIdSet.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CapacityManagerDataExportId"
              , CapacityManagerDataExportIdSet.to_query v.capacity_manager_data_export_ids
              ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some
           ( "CapacityManagerDataExportId"
           , CapacityManagerDataExportIdSet.to_json v.capacity_manager_data_export_ids )
       ])

let of_json j =
  { capacity_manager_data_export_ids =
      CapacityManagerDataExportIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityManagerDataExportId"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
