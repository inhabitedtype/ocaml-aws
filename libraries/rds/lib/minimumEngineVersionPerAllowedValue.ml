open Aws.BaseTypes

type t =
  { allowed_value : String.t option
  ; minimum_engine_version : String.t option
  }

let make ?allowed_value ?minimum_engine_version () =
  { allowed_value; minimum_engine_version }

let parse xml =
  Some
    { allowed_value =
        Aws.Util.option_bind (Aws.Xml.member "AllowedValue" xml) String.parse
    ; minimum_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "MinimumEngineVersion" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.minimum_engine_version (fun f ->
             Aws.Query.Pair ("MinimumEngineVersion", String.to_query f))
       ; Aws.Util.option_map v.allowed_value (fun f ->
             Aws.Query.Pair ("AllowedValue", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.minimum_engine_version (fun f ->
             "MinimumEngineVersion", String.to_json f)
       ; Aws.Util.option_map v.allowed_value (fun f -> "AllowedValue", String.to_json f)
       ])

let of_json j =
  { allowed_value = Aws.Util.option_map (Aws.Json.lookup j "AllowedValue") String.of_json
  ; minimum_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "MinimumEngineVersion") String.of_json
  }
