open Aws.BaseTypes

type t =
  { group_ids : GroupIdStringList.t
  ; group_names : GroupNameStringList.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  ; dry_run : Boolean.t option
  ; filters : FilterList.t
  }

let make
    ?(group_ids = [])
    ?(group_names = [])
    ?next_token
    ?max_results
    ?dry_run
    ?(filters = [])
    () =
  { group_ids; group_names; next_token; max_results; dry_run; filters }

let parse xml =
  Some
    { group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "GroupId" xml) GroupIdStringList.parse)
    ; group_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "GroupName" xml)
             GroupNameStringList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
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
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("GroupName", GroupNameStringList.to_query v.group_names))
       ; Some (Aws.Query.Pair ("GroupId", GroupIdStringList.to_query v.group_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("GroupName", GroupNameStringList.to_json v.group_names)
       ; Some ("GroupId", GroupIdStringList.to_json v.group_ids)
       ])

let of_json j =
  { group_ids =
      GroupIdStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GroupId"))
  ; group_names =
      GroupNameStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GroupName"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
