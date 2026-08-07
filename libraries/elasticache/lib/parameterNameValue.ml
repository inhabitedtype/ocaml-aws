open Aws.BaseTypes

type t =
  { parameter_name : String.t option
  ; parameter_value : String.t option
  }

let make ?parameter_name ?parameter_value () = { parameter_name; parameter_value }

let parse xml =
  Some
    { parameter_name =
        Aws.Util.option_bind (Aws.Xml.member "ParameterName" xml) String.parse
    ; parameter_value =
        Aws.Util.option_bind (Aws.Xml.member "ParameterValue" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.parameter_value (fun f ->
             Aws.Query.Pair ("ParameterValue", String.to_query f))
       ; Aws.Util.option_map v.parameter_name (fun f ->
             Aws.Query.Pair ("ParameterName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.parameter_value (fun f ->
             "ParameterValue", String.to_json f)
       ; Aws.Util.option_map v.parameter_name (fun f -> "ParameterName", String.to_json f)
       ])

let of_json j =
  { parameter_name =
      Aws.Util.option_map (Aws.Json.lookup j "ParameterName") String.of_json
  ; parameter_value =
      Aws.Util.option_map (Aws.Json.lookup j "ParameterValue") String.of_json
  }
