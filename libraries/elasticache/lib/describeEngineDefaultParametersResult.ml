type t = { engine_defaults : EngineDefaults.t }

let make ~engine_defaults () = { engine_defaults }

let parse xml =
  Some
    { engine_defaults =
        Aws.Xml.required
          "EngineDefaults"
          (Aws.Util.option_bind
             (Aws.Xml.member "EngineDefaults" xml)
             EngineDefaults.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("EngineDefaults", EngineDefaults.to_query v.engine_defaults))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("EngineDefaults", EngineDefaults.to_json v.engine_defaults) ])

let of_json j =
  { engine_defaults =
      EngineDefaults.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EngineDefaults"))
  }
