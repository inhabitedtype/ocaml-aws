open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; instance_types : RequestInstanceTypeList.t
  ; filters : FilterList.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; include_unsupported_in_region : Boolean.t option
  }

let make
    ?dry_run
    ?(instance_types = [])
    ?(filters = [])
    ?max_results
    ?next_token
    ?include_unsupported_in_region
    () =
  { dry_run
  ; instance_types
  ; filters
  ; max_results
  ; next_token
  ; include_unsupported_in_region
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; instance_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceType" xml)
             RequestInstanceTypeList.parse)
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; include_unsupported_in_region =
        Aws.Util.option_bind
          (Aws.Xml.member "IncludeUnsupportedInRegion" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_unsupported_in_region (fun f ->
             Aws.Query.Pair ("IncludeUnsupportedInRegion", Boolean.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Some
           (Aws.Query.Pair
              ("InstanceType", RequestInstanceTypeList.to_query v.instance_types))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_unsupported_in_region (fun f ->
             "IncludeUnsupportedInRegion", Boolean.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Some ("InstanceType", RequestInstanceTypeList.to_json v.instance_types)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; instance_types =
      RequestInstanceTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceType"))
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; include_unsupported_in_region =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeUnsupportedInRegion") Boolean.of_json
  }
