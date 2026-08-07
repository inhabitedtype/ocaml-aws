open Aws.BaseTypes

type t =
  { serverless_v2_platform_version : String.t option
  ; engine : String.t option
  ; filters : FilterList.t
  ; default_only : Boolean.t option
  ; include_all : Boolean.t option
  ; max_records : Integer.t option
  ; marker : String.t option
  }

let make
    ?serverless_v2_platform_version
    ?engine
    ?(filters = [])
    ?default_only
    ?include_all
    ?max_records
    ?marker
    () =
  { serverless_v2_platform_version
  ; engine
  ; filters
  ; default_only
  ; include_all
  ; max_records
  ; marker
  }

let parse xml =
  Some
    { serverless_v2_platform_version =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessV2PlatformVersion" xml)
          String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) FilterList.parse)
    ; default_only = Aws.Util.option_bind (Aws.Xml.member "DefaultOnly" xml) Boolean.parse
    ; include_all = Aws.Util.option_bind (Aws.Xml.member "IncludeAll" xml) Boolean.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.include_all (fun f ->
             Aws.Query.Pair ("IncludeAll", Boolean.to_query f))
       ; Aws.Util.option_map v.default_only (fun f ->
             Aws.Query.Pair ("DefaultOnly", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Filters.member", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.serverless_v2_platform_version (fun f ->
             Aws.Query.Pair ("ServerlessV2PlatformVersion", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.include_all (fun f -> "IncludeAll", Boolean.to_json f)
       ; Aws.Util.option_map v.default_only (fun f -> "DefaultOnly", Boolean.to_json f)
       ; Some ("Filters", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.serverless_v2_platform_version (fun f ->
             "ServerlessV2PlatformVersion", String.to_json f)
       ])

let of_json j =
  { serverless_v2_platform_version =
      Aws.Util.option_map (Aws.Json.lookup j "ServerlessV2PlatformVersion") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; default_only = Aws.Util.option_map (Aws.Json.lookup j "DefaultOnly") Boolean.of_json
  ; include_all = Aws.Util.option_map (Aws.Json.lookup j "IncludeAll") Boolean.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
