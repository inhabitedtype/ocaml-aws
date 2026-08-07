open Aws.BaseTypes

type t =
  { region_concurrency_type : RegionConcurrencyType.t option
  ; region_order : RegionList.t
  ; failure_tolerance_count : Integer.t option
  ; failure_tolerance_percentage : Integer.t option
  ; max_concurrent_count : Integer.t option
  ; max_concurrent_percentage : Integer.t option
  ; concurrency_mode : ConcurrencyMode.t option
  }

let make
    ?region_concurrency_type
    ?(region_order = [])
    ?failure_tolerance_count
    ?failure_tolerance_percentage
    ?max_concurrent_count
    ?max_concurrent_percentage
    ?concurrency_mode
    () =
  { region_concurrency_type
  ; region_order
  ; failure_tolerance_count
  ; failure_tolerance_percentage
  ; max_concurrent_count
  ; max_concurrent_percentage
  ; concurrency_mode
  }

let parse xml =
  Some
    { region_concurrency_type =
        Aws.Util.option_bind
          (Aws.Xml.member "RegionConcurrencyType" xml)
          RegionConcurrencyType.parse
    ; region_order =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "RegionOrder" xml) RegionList.parse)
    ; failure_tolerance_count =
        Aws.Util.option_bind (Aws.Xml.member "FailureToleranceCount" xml) Integer.parse
    ; failure_tolerance_percentage =
        Aws.Util.option_bind
          (Aws.Xml.member "FailureTolerancePercentage" xml)
          Integer.parse
    ; max_concurrent_count =
        Aws.Util.option_bind (Aws.Xml.member "MaxConcurrentCount" xml) Integer.parse
    ; max_concurrent_percentage =
        Aws.Util.option_bind (Aws.Xml.member "MaxConcurrentPercentage" xml) Integer.parse
    ; concurrency_mode =
        Aws.Util.option_bind (Aws.Xml.member "ConcurrencyMode" xml) ConcurrencyMode.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.concurrency_mode (fun f ->
             Aws.Query.Pair ("ConcurrencyMode", ConcurrencyMode.to_query f))
       ; Aws.Util.option_map v.max_concurrent_percentage (fun f ->
             Aws.Query.Pair ("MaxConcurrentPercentage", Integer.to_query f))
       ; Aws.Util.option_map v.max_concurrent_count (fun f ->
             Aws.Query.Pair ("MaxConcurrentCount", Integer.to_query f))
       ; Aws.Util.option_map v.failure_tolerance_percentage (fun f ->
             Aws.Query.Pair ("FailureTolerancePercentage", Integer.to_query f))
       ; Aws.Util.option_map v.failure_tolerance_count (fun f ->
             Aws.Query.Pair ("FailureToleranceCount", Integer.to_query f))
       ; Some (Aws.Query.Pair ("RegionOrder.member", RegionList.to_query v.region_order))
       ; Aws.Util.option_map v.region_concurrency_type (fun f ->
             Aws.Query.Pair ("RegionConcurrencyType", RegionConcurrencyType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.concurrency_mode (fun f ->
             "ConcurrencyMode", ConcurrencyMode.to_json f)
       ; Aws.Util.option_map v.max_concurrent_percentage (fun f ->
             "MaxConcurrentPercentage", Integer.to_json f)
       ; Aws.Util.option_map v.max_concurrent_count (fun f ->
             "MaxConcurrentCount", Integer.to_json f)
       ; Aws.Util.option_map v.failure_tolerance_percentage (fun f ->
             "FailureTolerancePercentage", Integer.to_json f)
       ; Aws.Util.option_map v.failure_tolerance_count (fun f ->
             "FailureToleranceCount", Integer.to_json f)
       ; Some ("RegionOrder", RegionList.to_json v.region_order)
       ; Aws.Util.option_map v.region_concurrency_type (fun f ->
             "RegionConcurrencyType", RegionConcurrencyType.to_json f)
       ])

let of_json j =
  { region_concurrency_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "RegionConcurrencyType")
        RegionConcurrencyType.of_json
  ; region_order =
      RegionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RegionOrder"))
  ; failure_tolerance_count =
      Aws.Util.option_map (Aws.Json.lookup j "FailureToleranceCount") Integer.of_json
  ; failure_tolerance_percentage =
      Aws.Util.option_map (Aws.Json.lookup j "FailureTolerancePercentage") Integer.of_json
  ; max_concurrent_count =
      Aws.Util.option_map (Aws.Json.lookup j "MaxConcurrentCount") Integer.of_json
  ; max_concurrent_percentage =
      Aws.Util.option_map (Aws.Json.lookup j "MaxConcurrentPercentage") Integer.of_json
  ; concurrency_mode =
      Aws.Util.option_map (Aws.Json.lookup j "ConcurrencyMode") ConcurrencyMode.of_json
  }
