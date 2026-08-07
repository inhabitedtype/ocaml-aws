open Aws.BaseTypes

type t =
  { filters : FilterList.t
  ; instance_ids : InstanceIdStringList.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; dry_run : Boolean.t option
  }

let make ?(filters = []) ?(instance_ids = []) ?max_results ?next_token ?dry_run () =
  { filters; instance_ids; max_results; next_token; dry_run }

let parse xml =
  Some
    { filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; instance_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceId" xml)
             InstanceIdStringList.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some
           (Aws.Query.Pair ("InstanceId", InstanceIdStringList.to_query v.instance_ids))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some ("InstanceId", InstanceIdStringList.to_json v.instance_ids)
       ; Some ("Filter", FilterList.to_json v.filters)
       ])

let of_json j =
  { filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; instance_ids =
      InstanceIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
