open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; filters : FilterList.t
  ; exclusion_ids : VpcBlockPublicAccessExclusionIdList.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make ?dry_run ?(filters = []) ?(exclusion_ids = []) ?next_token ?max_results () =
  { dry_run; filters; exclusion_ids; next_token; max_results }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; exclusion_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ExclusionId" xml)
             VpcBlockPublicAccessExclusionIdList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ExclusionId", VpcBlockPublicAccessExclusionIdList.to_query v.exclusion_ids))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("ExclusionId", VpcBlockPublicAccessExclusionIdList.to_json v.exclusion_ids)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; exclusion_ids =
      VpcBlockPublicAccessExclusionIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ExclusionId"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
