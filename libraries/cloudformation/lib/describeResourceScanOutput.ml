open Aws.BaseTypes

type t =
  { resource_scan_id : String.t option
  ; status : ResourceScanStatus.t option
  ; status_reason : String.t option
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; percentage_completed : Double.t option
  ; resource_types : ResourceTypes.t
  ; resources_scanned : Integer.t option
  ; resources_read : Integer.t option
  ; scan_filters : ScanFilters.t
  }

let make
    ?resource_scan_id
    ?status
    ?status_reason
    ?start_time
    ?end_time
    ?percentage_completed
    ?(resource_types = [])
    ?resources_scanned
    ?resources_read
    ?(scan_filters = [])
    () =
  { resource_scan_id
  ; status
  ; status_reason
  ; start_time
  ; end_time
  ; percentage_completed
  ; resource_types
  ; resources_scanned
  ; resources_read
  ; scan_filters
  }

let parse xml =
  Some
    { resource_scan_id =
        Aws.Util.option_bind (Aws.Xml.member "ResourceScanId" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) ResourceScanStatus.parse
    ; status_reason =
        Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
    ; percentage_completed =
        Aws.Util.option_bind (Aws.Xml.member "PercentageCompleted" xml) Double.parse
    ; resource_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ResourceTypes" xml) ResourceTypes.parse)
    ; resources_scanned =
        Aws.Util.option_bind (Aws.Xml.member "ResourcesScanned" xml) Integer.parse
    ; resources_read =
        Aws.Util.option_bind (Aws.Xml.member "ResourcesRead" xml) Integer.parse
    ; scan_filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ScanFilters" xml) ScanFilters.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ScanFilters.member", ScanFilters.to_query v.scan_filters))
       ; Aws.Util.option_map v.resources_read (fun f ->
             Aws.Query.Pair ("ResourcesRead", Integer.to_query f))
       ; Aws.Util.option_map v.resources_scanned (fun f ->
             Aws.Query.Pair ("ResourcesScanned", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ResourceTypes.member", ResourceTypes.to_query v.resource_types))
       ; Aws.Util.option_map v.percentage_completed (fun f ->
             Aws.Query.Pair ("PercentageCompleted", Double.to_query f))
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.status_reason (fun f ->
             Aws.Query.Pair ("StatusReason", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ResourceScanStatus.to_query f))
       ; Aws.Util.option_map v.resource_scan_id (fun f ->
             Aws.Query.Pair ("ResourceScanId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ScanFilters", ScanFilters.to_json v.scan_filters)
       ; Aws.Util.option_map v.resources_read (fun f ->
             "ResourcesRead", Integer.to_json f)
       ; Aws.Util.option_map v.resources_scanned (fun f ->
             "ResourcesScanned", Integer.to_json f)
       ; Some ("ResourceTypes", ResourceTypes.to_json v.resource_types)
       ; Aws.Util.option_map v.percentage_completed (fun f ->
             "PercentageCompleted", Double.to_json f)
       ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", ResourceScanStatus.to_json f)
       ; Aws.Util.option_map v.resource_scan_id (fun f ->
             "ResourceScanId", String.to_json f)
       ])

let of_json j =
  { resource_scan_id =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceScanId") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") ResourceScanStatus.of_json
  ; status_reason = Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
  ; percentage_completed =
      Aws.Util.option_map (Aws.Json.lookup j "PercentageCompleted") Double.of_json
  ; resource_types =
      ResourceTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTypes"))
  ; resources_scanned =
      Aws.Util.option_map (Aws.Json.lookup j "ResourcesScanned") Integer.of_json
  ; resources_read =
      Aws.Util.option_map (Aws.Json.lookup j "ResourcesRead") Integer.of_json
  ; scan_filters =
      ScanFilters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ScanFilters"))
  }
