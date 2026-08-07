open Aws.BaseTypes

type t =
  { serverless_cache_name : String.t option
  ; engine : String.t option
  ; major_engine_version : String.t option
  }

let make ?serverless_cache_name ?engine ?major_engine_version () =
  { serverless_cache_name; engine; major_engine_version }

let parse xml =
  Some
    { serverless_cache_name =
        Aws.Util.option_bind (Aws.Xml.member "ServerlessCacheName" xml) String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; major_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "MajorEngineVersion" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.major_engine_version (fun f ->
             Aws.Query.Pair ("MajorEngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.serverless_cache_name (fun f ->
             Aws.Query.Pair ("ServerlessCacheName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.major_engine_version (fun f ->
             "MajorEngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.serverless_cache_name (fun f ->
             "ServerlessCacheName", String.to_json f)
       ])

let of_json j =
  { serverless_cache_name =
      Aws.Util.option_map (Aws.Json.lookup j "ServerlessCacheName") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; major_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "MajorEngineVersion") String.of_json
  }
