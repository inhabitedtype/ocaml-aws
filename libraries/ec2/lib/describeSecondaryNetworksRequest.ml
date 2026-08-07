open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; filters : FilterList.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; secondary_network_ids : SecondaryNetworkIdList.t
  }

let make
    ?dry_run
    ?(filters = [])
    ?max_results
    ?next_token
    ?(secondary_network_ids = [])
    () =
  { dry_run; filters; max_results; next_token; secondary_network_ids }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; secondary_network_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecondaryNetworkId" xml)
             SecondaryNetworkIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SecondaryNetworkId"
              , SecondaryNetworkIdList.to_query v.secondary_network_ids ))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("SecondaryNetworkId", SecondaryNetworkIdList.to_json v.secondary_network_ids)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; secondary_network_ids =
      SecondaryNetworkIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecondaryNetworkId"))
  }
