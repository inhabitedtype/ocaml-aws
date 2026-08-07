open Aws.BaseTypes

type t =
  { engine : String.t option
  ; engine_version : String.t option
  ; cache_parameter_group_family : String.t option
  ; max_records : Integer.t option
  ; marker : String.t option
  ; default_only : Boolean.t option
  }

let make
    ?engine
    ?engine_version
    ?cache_parameter_group_family
    ?max_records
    ?marker
    ?default_only
    () =
  { engine
  ; engine_version
  ; cache_parameter_group_family
  ; max_records
  ; marker
  ; default_only
  }

let parse xml =
  Some
    { engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; cache_parameter_group_family =
        Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupFamily" xml) String.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; default_only = Aws.Util.option_bind (Aws.Xml.member "DefaultOnly" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.default_only (fun f ->
             Aws.Query.Pair ("DefaultOnly", Boolean.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.cache_parameter_group_family (fun f ->
             Aws.Query.Pair ("CacheParameterGroupFamily", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.default_only (fun f -> "DefaultOnly", Boolean.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.cache_parameter_group_family (fun f ->
             "CacheParameterGroupFamily", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ])

let of_json j =
  { engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; cache_parameter_group_family =
      Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupFamily") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; default_only = Aws.Util.option_map (Aws.Json.lookup j "DefaultOnly") Boolean.of_json
  }
