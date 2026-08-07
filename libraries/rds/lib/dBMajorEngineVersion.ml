open Aws.BaseTypes

type t =
  { engine : String.t option
  ; major_engine_version : String.t option
  ; supported_engine_lifecycles : SupportedEngineLifecycleList.t
  }

let make ?engine ?major_engine_version ?(supported_engine_lifecycles = []) () =
  { engine; major_engine_version; supported_engine_lifecycles }

let parse xml =
  Some
    { engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; major_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "MajorEngineVersion" xml) String.parse
    ; supported_engine_lifecycles =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedEngineLifecycles" xml)
             SupportedEngineLifecycleList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SupportedEngineLifecycles.member"
              , SupportedEngineLifecycleList.to_query v.supported_engine_lifecycles ))
       ; Aws.Util.option_map v.major_engine_version (fun f ->
             Aws.Query.Pair ("MajorEngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "SupportedEngineLifecycles"
           , SupportedEngineLifecycleList.to_json v.supported_engine_lifecycles )
       ; Aws.Util.option_map v.major_engine_version (fun f ->
             "MajorEngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ])

let of_json j =
  { engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; major_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "MajorEngineVersion") String.of_json
  ; supported_engine_lifecycles =
      SupportedEngineLifecycleList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedEngineLifecycles"))
  }
