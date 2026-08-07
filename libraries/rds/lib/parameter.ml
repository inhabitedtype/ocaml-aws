open Aws.BaseTypes

type t =
  { parameter_name : String.t option
  ; parameter_value : String.t option
  ; description : String.t option
  ; source : String.t option
  ; apply_type : String.t option
  ; data_type : String.t option
  ; allowed_values : String.t option
  ; is_modifiable : Boolean.t option
  ; minimum_engine_version : String.t option
  ; apply_method : ApplyMethod.t option
  ; supported_engine_modes : EngineModeList.t
  }

let make
    ?parameter_name
    ?parameter_value
    ?description
    ?source
    ?apply_type
    ?data_type
    ?allowed_values
    ?is_modifiable
    ?minimum_engine_version
    ?apply_method
    ?(supported_engine_modes = [])
    () =
  { parameter_name
  ; parameter_value
  ; description
  ; source
  ; apply_type
  ; data_type
  ; allowed_values
  ; is_modifiable
  ; minimum_engine_version
  ; apply_method
  ; supported_engine_modes
  }

let parse xml =
  Some
    { parameter_name =
        Aws.Util.option_bind (Aws.Xml.member "ParameterName" xml) String.parse
    ; parameter_value =
        Aws.Util.option_bind (Aws.Xml.member "ParameterValue" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; source = Aws.Util.option_bind (Aws.Xml.member "Source" xml) String.parse
    ; apply_type = Aws.Util.option_bind (Aws.Xml.member "ApplyType" xml) String.parse
    ; data_type = Aws.Util.option_bind (Aws.Xml.member "DataType" xml) String.parse
    ; allowed_values =
        Aws.Util.option_bind (Aws.Xml.member "AllowedValues" xml) String.parse
    ; is_modifiable =
        Aws.Util.option_bind (Aws.Xml.member "IsModifiable" xml) Boolean.parse
    ; minimum_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "MinimumEngineVersion" xml) String.parse
    ; apply_method =
        Aws.Util.option_bind (Aws.Xml.member "ApplyMethod" xml) ApplyMethod.parse
    ; supported_engine_modes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedEngineModes" xml)
             EngineModeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SupportedEngineModes.member"
              , EngineModeList.to_query v.supported_engine_modes ))
       ; Aws.Util.option_map v.apply_method (fun f ->
             Aws.Query.Pair ("ApplyMethod", ApplyMethod.to_query f))
       ; Aws.Util.option_map v.minimum_engine_version (fun f ->
             Aws.Query.Pair ("MinimumEngineVersion", String.to_query f))
       ; Aws.Util.option_map v.is_modifiable (fun f ->
             Aws.Query.Pair ("IsModifiable", Boolean.to_query f))
       ; Aws.Util.option_map v.allowed_values (fun f ->
             Aws.Query.Pair ("AllowedValues", String.to_query f))
       ; Aws.Util.option_map v.data_type (fun f ->
             Aws.Query.Pair ("DataType", String.to_query f))
       ; Aws.Util.option_map v.apply_type (fun f ->
             Aws.Query.Pair ("ApplyType", String.to_query f))
       ; Aws.Util.option_map v.source (fun f ->
             Aws.Query.Pair ("Source", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.parameter_value (fun f ->
             Aws.Query.Pair ("ParameterValue", String.to_query f))
       ; Aws.Util.option_map v.parameter_name (fun f ->
             Aws.Query.Pair ("ParameterName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SupportedEngineModes", EngineModeList.to_json v.supported_engine_modes)
       ; Aws.Util.option_map v.apply_method (fun f ->
             "ApplyMethod", ApplyMethod.to_json f)
       ; Aws.Util.option_map v.minimum_engine_version (fun f ->
             "MinimumEngineVersion", String.to_json f)
       ; Aws.Util.option_map v.is_modifiable (fun f -> "IsModifiable", Boolean.to_json f)
       ; Aws.Util.option_map v.allowed_values (fun f -> "AllowedValues", String.to_json f)
       ; Aws.Util.option_map v.data_type (fun f -> "DataType", String.to_json f)
       ; Aws.Util.option_map v.apply_type (fun f -> "ApplyType", String.to_json f)
       ; Aws.Util.option_map v.source (fun f -> "Source", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.parameter_value (fun f ->
             "ParameterValue", String.to_json f)
       ; Aws.Util.option_map v.parameter_name (fun f -> "ParameterName", String.to_json f)
       ])

let of_json j =
  { parameter_name =
      Aws.Util.option_map (Aws.Json.lookup j "ParameterName") String.of_json
  ; parameter_value =
      Aws.Util.option_map (Aws.Json.lookup j "ParameterValue") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; source = Aws.Util.option_map (Aws.Json.lookup j "Source") String.of_json
  ; apply_type = Aws.Util.option_map (Aws.Json.lookup j "ApplyType") String.of_json
  ; data_type = Aws.Util.option_map (Aws.Json.lookup j "DataType") String.of_json
  ; allowed_values =
      Aws.Util.option_map (Aws.Json.lookup j "AllowedValues") String.of_json
  ; is_modifiable = Aws.Util.option_map (Aws.Json.lookup j "IsModifiable") Boolean.of_json
  ; minimum_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "MinimumEngineVersion") String.of_json
  ; apply_method =
      Aws.Util.option_map (Aws.Json.lookup j "ApplyMethod") ApplyMethod.of_json
  ; supported_engine_modes =
      EngineModeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedEngineModes"))
  }
