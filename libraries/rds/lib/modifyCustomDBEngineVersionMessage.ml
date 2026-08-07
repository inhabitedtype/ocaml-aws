open Aws.BaseTypes

type t =
  { engine : String.t
  ; engine_version : String.t
  ; description : String.t option
  ; status : CustomEngineVersionStatus.t option
  }

let make ~engine ~engine_version ?description ?status () =
  { engine; engine_version; description; status }

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
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) CustomEngineVersionStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", CustomEngineVersionStatus.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some (Aws.Query.Pair ("EngineVersion", String.to_query v.engine_version))
       ; Some (Aws.Query.Pair ("Engine", String.to_query v.engine))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             "Status", CustomEngineVersionStatus.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("EngineVersion", String.to_json v.engine_version)
       ; Some ("Engine", String.to_json v.engine)
       ])

let of_json j =
  { engine = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Engine"))
  ; engine_version =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EngineVersion"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "Status") CustomEngineVersionStatus.of_json
  }
