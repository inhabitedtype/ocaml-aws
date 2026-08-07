open Aws.BaseTypes

type t =
  { report_id : String.t option
  ; s3_bucket : String.t option
  ; s3_prefix : String.t option
  ; target_id : String.t option
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; status : ReportState.t option
  ; tags : TagList.t
  }

let make
    ?report_id
    ?s3_bucket
    ?s3_prefix
    ?target_id
    ?start_time
    ?end_time
    ?status
    ?(tags = [])
    () =
  { report_id; s3_bucket; s3_prefix; target_id; start_time; end_time; status; tags }

let parse xml =
  Some
    { report_id = Aws.Util.option_bind (Aws.Xml.member "reportId" xml) String.parse
    ; s3_bucket = Aws.Util.option_bind (Aws.Xml.member "s3Bucket" xml) String.parse
    ; s3_prefix = Aws.Util.option_bind (Aws.Xml.member "s3Prefix" xml) String.parse
    ; target_id = Aws.Util.option_bind (Aws.Xml.member "targetId" xml) String.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "startTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "endTime" xml) DateTime.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) ReportState.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ReportState.to_query f))
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.target_id (fun f ->
             Aws.Query.Pair ("TargetId", String.to_query f))
       ; Aws.Util.option_map v.s3_prefix (fun f ->
             Aws.Query.Pair ("S3Prefix", String.to_query f))
       ; Aws.Util.option_map v.s3_bucket (fun f ->
             Aws.Query.Pair ("S3Bucket", String.to_query f))
       ; Aws.Util.option_map v.report_id (fun f ->
             Aws.Query.Pair ("ReportId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.status (fun f -> "status", ReportState.to_json f)
       ; Aws.Util.option_map v.end_time (fun f -> "endTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "startTime", DateTime.to_json f)
       ; Aws.Util.option_map v.target_id (fun f -> "targetId", String.to_json f)
       ; Aws.Util.option_map v.s3_prefix (fun f -> "s3Prefix", String.to_json f)
       ; Aws.Util.option_map v.s3_bucket (fun f -> "s3Bucket", String.to_json f)
       ; Aws.Util.option_map v.report_id (fun f -> "reportId", String.to_json f)
       ])

let of_json j =
  { report_id = Aws.Util.option_map (Aws.Json.lookup j "reportId") String.of_json
  ; s3_bucket = Aws.Util.option_map (Aws.Json.lookup j "s3Bucket") String.of_json
  ; s3_prefix = Aws.Util.option_map (Aws.Json.lookup j "s3Prefix") String.of_json
  ; target_id = Aws.Util.option_map (Aws.Json.lookup j "targetId") String.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "startTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "endTime") DateTime.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") ReportState.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
