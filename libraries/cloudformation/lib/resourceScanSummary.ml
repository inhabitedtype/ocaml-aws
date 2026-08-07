open Aws.BaseTypes

type t =
  { resource_scan_id : String.t option
  ; status : ResourceScanStatus.t option
  ; status_reason : String.t option
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; percentage_completed : Double.t option
  ; scan_type : ScanType.t option
  }

let make
    ?resource_scan_id
    ?status
    ?status_reason
    ?start_time
    ?end_time
    ?percentage_completed
    ?scan_type
    () =
  { resource_scan_id
  ; status
  ; status_reason
  ; start_time
  ; end_time
  ; percentage_completed
  ; scan_type
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
    ; scan_type = Aws.Util.option_bind (Aws.Xml.member "ScanType" xml) ScanType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scan_type (fun f ->
             Aws.Query.Pair ("ScanType", ScanType.to_query f))
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
       [ Aws.Util.option_map v.scan_type (fun f -> "ScanType", ScanType.to_json f)
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
  ; scan_type = Aws.Util.option_map (Aws.Json.lookup j "ScanType") ScanType.of_json
  }
