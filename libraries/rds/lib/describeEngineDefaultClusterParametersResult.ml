type t = { engine_defaults : EngineDefaults.t option }

let make ?engine_defaults () = { engine_defaults }

let parse xml =
  Some
    { engine_defaults =
        Aws.Util.option_bind (Aws.Xml.member "EngineDefaults" xml) EngineDefaults.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.engine_defaults (fun f ->
             Aws.Query.Pair ("EngineDefaults", EngineDefaults.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.engine_defaults (fun f ->
             "EngineDefaults", EngineDefaults.to_json f)
       ])

let of_json j =
  { engine_defaults =
      Aws.Util.option_map (Aws.Json.lookup j "EngineDefaults") EngineDefaults.of_json
  }
