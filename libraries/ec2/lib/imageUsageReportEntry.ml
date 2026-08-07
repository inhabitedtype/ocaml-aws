open Aws.BaseTypes

type t =
  { resource_type : String.t option
  ; report_id : String.t option
  ; usage_count : Long.t option
  ; account_id : String.t option
  ; image_id : String.t option
  ; report_creation_time : DateTime.t option
  }

let make
    ?resource_type
    ?report_id
    ?usage_count
    ?account_id
    ?image_id
    ?report_creation_time
    () =
  { resource_type; report_id; usage_count; account_id; image_id; report_creation_time }

let parse xml =
  Some
    { resource_type =
        Aws.Util.option_bind (Aws.Xml.member "resourceType" xml) String.parse
    ; report_id = Aws.Util.option_bind (Aws.Xml.member "reportId" xml) String.parse
    ; usage_count = Aws.Util.option_bind (Aws.Xml.member "usageCount" xml) Long.parse
    ; account_id = Aws.Util.option_bind (Aws.Xml.member "accountId" xml) String.parse
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse
    ; report_creation_time =
        Aws.Util.option_bind (Aws.Xml.member "reportCreationTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.report_creation_time (fun f ->
             Aws.Query.Pair ("ReportCreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Aws.Util.option_map v.account_id (fun f ->
             Aws.Query.Pair ("AccountId", String.to_query f))
       ; Aws.Util.option_map v.usage_count (fun f ->
             Aws.Query.Pair ("UsageCount", Long.to_query f))
       ; Aws.Util.option_map v.report_id (fun f ->
             Aws.Query.Pair ("ReportId", String.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.report_creation_time (fun f ->
             "reportCreationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f)
       ; Aws.Util.option_map v.account_id (fun f -> "accountId", String.to_json f)
       ; Aws.Util.option_map v.usage_count (fun f -> "usageCount", Long.to_json f)
       ; Aws.Util.option_map v.report_id (fun f -> "reportId", String.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f -> "resourceType", String.to_json f)
       ])

let of_json j =
  { resource_type = Aws.Util.option_map (Aws.Json.lookup j "resourceType") String.of_json
  ; report_id = Aws.Util.option_map (Aws.Json.lookup j "reportId") String.of_json
  ; usage_count = Aws.Util.option_map (Aws.Json.lookup j "usageCount") Long.of_json
  ; account_id = Aws.Util.option_map (Aws.Json.lookup j "accountId") String.of_json
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json
  ; report_creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "reportCreationTime") DateTime.of_json
  }
