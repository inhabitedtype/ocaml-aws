open Aws.BaseTypes

type t =
  { export_task_identifier : String.t option
  ; source_arn : String.t option
  ; filters : FilterList.t
  ; marker : String.t option
  ; max_records : Integer.t option
  ; source_type : ExportSourceType.t option
  }

let make
    ?export_task_identifier
    ?source_arn
    ?(filters = [])
    ?marker
    ?max_records
    ?source_type
    () =
  { export_task_identifier; source_arn; filters; marker; max_records; source_type }

let parse xml =
  Some
    { export_task_identifier =
        Aws.Util.option_bind (Aws.Xml.member "ExportTaskIdentifier" xml) String.parse
    ; source_arn = Aws.Util.option_bind (Aws.Xml.member "SourceArn" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) FilterList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; source_type =
        Aws.Util.option_bind (Aws.Xml.member "SourceType" xml) ExportSourceType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_type (fun f ->
             Aws.Query.Pair ("SourceType", ExportSourceType.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some (Aws.Query.Pair ("Filters.member", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.source_arn (fun f ->
             Aws.Query.Pair ("SourceArn", String.to_query f))
       ; Aws.Util.option_map v.export_task_identifier (fun f ->
             Aws.Query.Pair ("ExportTaskIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_type (fun f ->
             "SourceType", ExportSourceType.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("Filters", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.source_arn (fun f -> "SourceArn", String.to_json f)
       ; Aws.Util.option_map v.export_task_identifier (fun f ->
             "ExportTaskIdentifier", String.to_json f)
       ])

let of_json j =
  { export_task_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "ExportTaskIdentifier") String.of_json
  ; source_arn = Aws.Util.option_map (Aws.Json.lookup j "SourceArn") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; source_type =
      Aws.Util.option_map (Aws.Json.lookup j "SourceType") ExportSourceType.of_json
  }
