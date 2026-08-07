open Aws.BaseTypes

type t =
  { parameter_key : String.t option
  ; parameter_value : String.t option
  ; use_previous_value : Boolean.t option
  ; resolved_value : String.t option
  }

let make ?parameter_key ?parameter_value ?use_previous_value ?resolved_value () =
  { parameter_key; parameter_value; use_previous_value; resolved_value }

let parse xml =
  Some
    { parameter_key =
        Aws.Util.option_bind (Aws.Xml.member "ParameterKey" xml) String.parse
    ; parameter_value =
        Aws.Util.option_bind (Aws.Xml.member "ParameterValue" xml) String.parse
    ; use_previous_value =
        Aws.Util.option_bind (Aws.Xml.member "UsePreviousValue" xml) Boolean.parse
    ; resolved_value =
        Aws.Util.option_bind (Aws.Xml.member "ResolvedValue" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resolved_value (fun f ->
             Aws.Query.Pair ("ResolvedValue", String.to_query f))
       ; Aws.Util.option_map v.use_previous_value (fun f ->
             Aws.Query.Pair ("UsePreviousValue", Boolean.to_query f))
       ; Aws.Util.option_map v.parameter_value (fun f ->
             Aws.Query.Pair ("ParameterValue", String.to_query f))
       ; Aws.Util.option_map v.parameter_key (fun f ->
             Aws.Query.Pair ("ParameterKey", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resolved_value (fun f -> "ResolvedValue", String.to_json f)
       ; Aws.Util.option_map v.use_previous_value (fun f ->
             "UsePreviousValue", Boolean.to_json f)
       ; Aws.Util.option_map v.parameter_value (fun f ->
             "ParameterValue", String.to_json f)
       ; Aws.Util.option_map v.parameter_key (fun f -> "ParameterKey", String.to_json f)
       ])

let of_json j =
  { parameter_key = Aws.Util.option_map (Aws.Json.lookup j "ParameterKey") String.of_json
  ; parameter_value =
      Aws.Util.option_map (Aws.Json.lookup j "ParameterValue") String.of_json
  ; use_previous_value =
      Aws.Util.option_map (Aws.Json.lookup j "UsePreviousValue") Boolean.of_json
  ; resolved_value =
      Aws.Util.option_map (Aws.Json.lookup j "ResolvedValue") String.of_json
  }
