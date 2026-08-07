open Aws.BaseTypes

type t =
  { engine : String.t
  ; engine_version : String.t
  }

let make ~engine ~engine_version () = { engine; engine_version }

let parse xml =
  Some
    { engine =
        Aws.Xml.required
          "Engine"
          (Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse)
    ; engine_version =
        Aws.Xml.required
          "EngineVersion"
          (Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("EngineVersion", String.to_query v.engine_version))
       ; Some (Aws.Query.Pair ("Engine", String.to_query v.engine))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("EngineVersion", String.to_json v.engine_version)
       ; Some ("Engine", String.to_json v.engine)
       ])

let of_json j =
  { engine = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Engine"))
  ; engine_version =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EngineVersion"))
  }
