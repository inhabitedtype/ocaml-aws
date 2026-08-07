open Aws.BaseTypes

type t =
  { engine : String.t option
  ; engine_version : String.t option
  ; cache_parameter_group_family : String.t option
  ; cache_engine_description : String.t option
  ; cache_engine_version_description : String.t option
  }

let make
    ?engine
    ?engine_version
    ?cache_parameter_group_family
    ?cache_engine_description
    ?cache_engine_version_description
    () =
  { engine
  ; engine_version
  ; cache_parameter_group_family
  ; cache_engine_description
  ; cache_engine_version_description
  }

let parse xml =
  Some
    { engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; cache_parameter_group_family =
        Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupFamily" xml) String.parse
    ; cache_engine_description =
        Aws.Util.option_bind (Aws.Xml.member "CacheEngineDescription" xml) String.parse
    ; cache_engine_version_description =
        Aws.Util.option_bind
          (Aws.Xml.member "CacheEngineVersionDescription" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cache_engine_version_description (fun f ->
             Aws.Query.Pair ("CacheEngineVersionDescription", String.to_query f))
       ; Aws.Util.option_map v.cache_engine_description (fun f ->
             Aws.Query.Pair ("CacheEngineDescription", String.to_query f))
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
       [ Aws.Util.option_map v.cache_engine_version_description (fun f ->
             "CacheEngineVersionDescription", String.to_json f)
       ; Aws.Util.option_map v.cache_engine_description (fun f ->
             "CacheEngineDescription", String.to_json f)
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
  ; cache_engine_description =
      Aws.Util.option_map (Aws.Json.lookup j "CacheEngineDescription") String.of_json
  ; cache_engine_version_description =
      Aws.Util.option_map
        (Aws.Json.lookup j "CacheEngineVersionDescription")
        String.of_json
  }
