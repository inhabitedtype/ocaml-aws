open Aws.BaseTypes

type t =
  { parameter_key : String.t option
  ; default_value : String.t option
  ; no_echo : Boolean.t option
  ; description : String.t option
  }

let make ?parameter_key ?default_value ?no_echo ?description () =
  { parameter_key; default_value; no_echo; description }

let parse xml =
  Some
    { parameter_key =
        Aws.Util.option_bind (Aws.Xml.member "ParameterKey" xml) String.parse
    ; default_value =
        Aws.Util.option_bind (Aws.Xml.member "DefaultValue" xml) String.parse
    ; no_echo = Aws.Util.option_bind (Aws.Xml.member "NoEcho" xml) Boolean.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.no_echo (fun f ->
             Aws.Query.Pair ("NoEcho", Boolean.to_query f))
       ; Aws.Util.option_map v.default_value (fun f ->
             Aws.Query.Pair ("DefaultValue", String.to_query f))
       ; Aws.Util.option_map v.parameter_key (fun f ->
             Aws.Query.Pair ("ParameterKey", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.no_echo (fun f -> "NoEcho", Boolean.to_json f)
       ; Aws.Util.option_map v.default_value (fun f -> "DefaultValue", String.to_json f)
       ; Aws.Util.option_map v.parameter_key (fun f -> "ParameterKey", String.to_json f)
       ])

let of_json j =
  { parameter_key = Aws.Util.option_map (Aws.Json.lookup j "ParameterKey") String.of_json
  ; default_value = Aws.Util.option_map (Aws.Json.lookup j "DefaultValue") String.of_json
  ; no_echo = Aws.Util.option_map (Aws.Json.lookup j "NoEcho") Boolean.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  }
