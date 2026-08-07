open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; filters : FilterList.t
  ; mac_modification_task_ids : MacModificationTaskIdList.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make
    ?dry_run
    ?(filters = [])
    ?(mac_modification_task_ids = [])
    ?max_results
    ?next_token
    () =
  { dry_run; filters; mac_modification_task_ids; max_results; next_token }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; mac_modification_task_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "MacModificationTaskId" xml)
             MacModificationTaskIdList.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "MacModificationTaskId"
              , MacModificationTaskIdList.to_query v.mac_modification_task_ids ))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some
           ( "MacModificationTaskId"
           , MacModificationTaskIdList.to_json v.mac_modification_task_ids )
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; mac_modification_task_ids =
      MacModificationTaskIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MacModificationTaskId"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
