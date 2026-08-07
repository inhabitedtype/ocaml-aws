open Aws.BaseTypes

type t =
  { option_group_name : String.t option
  ; filters : FilterList.t
  ; marker : String.t option
  ; max_records : Integer.t option
  ; engine_name : String.t option
  ; major_engine_version : String.t option
  }

let make
    ?option_group_name
    ?(filters = [])
    ?marker
    ?max_records
    ?engine_name
    ?major_engine_version
    () =
  { option_group_name; filters; marker; max_records; engine_name; major_engine_version }

let parse xml =
  Some
    { option_group_name =
        Aws.Util.option_bind (Aws.Xml.member "OptionGroupName" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) FilterList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; engine_name = Aws.Util.option_bind (Aws.Xml.member "EngineName" xml) String.parse
    ; major_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "MajorEngineVersion" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.major_engine_version (fun f ->
             Aws.Query.Pair ("MajorEngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine_name (fun f ->
             Aws.Query.Pair ("EngineName", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some (Aws.Query.Pair ("Filters.member", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.option_group_name (fun f ->
             Aws.Query.Pair ("OptionGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.major_engine_version (fun f ->
             "MajorEngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine_name (fun f -> "EngineName", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("Filters", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.option_group_name (fun f ->
             "OptionGroupName", String.to_json f)
       ])

let of_json j =
  { option_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroupName") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; engine_name = Aws.Util.option_map (Aws.Json.lookup j "EngineName") String.of_json
  ; major_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "MajorEngineVersion") String.of_json
  }
