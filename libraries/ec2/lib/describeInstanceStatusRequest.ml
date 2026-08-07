open Aws.BaseTypes

type t =
  { instance_ids : InstanceIdStringList.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; include_managed_resources : Boolean.t option
  ; dry_run : Boolean.t option
  ; filters : FilterList.t
  ; include_all_instances : Boolean.t option
  }

let make
    ?(instance_ids = [])
    ?max_results
    ?next_token
    ?include_managed_resources
    ?dry_run
    ?(filters = [])
    ?include_all_instances
    () =
  { instance_ids
  ; max_results
  ; next_token
  ; include_managed_resources
  ; dry_run
  ; filters
  ; include_all_instances
  }

let parse xml =
  Some
    { instance_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceId" xml)
             InstanceIdStringList.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; include_managed_resources =
        Aws.Util.option_bind (Aws.Xml.member "IncludeManagedResources" xml) Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; include_all_instances =
        Aws.Util.option_bind (Aws.Xml.member "includeAllInstances" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_all_instances (fun f ->
             Aws.Query.Pair ("IncludeAllInstances", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.include_managed_resources (fun f ->
             Aws.Query.Pair ("IncludeManagedResources", Boolean.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some
           (Aws.Query.Pair ("InstanceId", InstanceIdStringList.to_query v.instance_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_all_instances (fun f ->
             "includeAllInstances", Boolean.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.include_managed_resources (fun f ->
             "IncludeManagedResources", Boolean.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some ("InstanceId", InstanceIdStringList.to_json v.instance_ids)
       ])

let of_json j =
  { instance_ids =
      InstanceIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; include_managed_resources =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeManagedResources") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; include_all_instances =
      Aws.Util.option_map (Aws.Json.lookup j "includeAllInstances") Boolean.of_json
  }
