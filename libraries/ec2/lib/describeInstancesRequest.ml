open Aws.BaseTypes

type t =
  { instance_ids : InstanceIdStringList.t
  ; include_managed_resources : Boolean.t option
  ; dry_run : Boolean.t option
  ; filters : FilterList.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make
    ?(instance_ids = [])
    ?include_managed_resources
    ?dry_run
    ?(filters = [])
    ?next_token
    ?max_results
    () =
  { instance_ids; include_managed_resources; dry_run; filters; next_token; max_results }

let parse xml =
  Some
    { instance_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceId" xml)
             InstanceIdStringList.parse)
    ; include_managed_resources =
        Aws.Util.option_bind (Aws.Xml.member "IncludeManagedResources" xml) Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "maxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.include_managed_resources (fun f ->
             Aws.Query.Pair ("IncludeManagedResources", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("InstanceId", InstanceIdStringList.to_query v.instance_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "maxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.include_managed_resources (fun f ->
             "IncludeManagedResources", Boolean.to_json f)
       ; Some ("InstanceId", InstanceIdStringList.to_json v.instance_ids)
       ])

let of_json j =
  { instance_ids =
      InstanceIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; include_managed_resources =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeManagedResources") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "maxResults") Integer.of_json
  }
