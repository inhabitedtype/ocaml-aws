open Aws.BaseTypes

type t =
  { error_code : String.t option
  ; error_message : String.t option
  ; type_configuration_identifier : TypeConfigurationIdentifier.t option
  }

let make ?error_code ?error_message ?type_configuration_identifier () =
  { error_code; error_message; type_configuration_identifier }

let parse xml =
  Some
    { error_code = Aws.Util.option_bind (Aws.Xml.member "ErrorCode" xml) String.parse
    ; error_message =
        Aws.Util.option_bind (Aws.Xml.member "ErrorMessage" xml) String.parse
    ; type_configuration_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "TypeConfigurationIdentifier" xml)
          TypeConfigurationIdentifier.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_configuration_identifier (fun f ->
             Aws.Query.Pair
               ("TypeConfigurationIdentifier", TypeConfigurationIdentifier.to_query f))
       ; Aws.Util.option_map v.error_message (fun f ->
             Aws.Query.Pair ("ErrorMessage", String.to_query f))
       ; Aws.Util.option_map v.error_code (fun f ->
             Aws.Query.Pair ("ErrorCode", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_configuration_identifier (fun f ->
             "TypeConfigurationIdentifier", TypeConfigurationIdentifier.to_json f)
       ; Aws.Util.option_map v.error_message (fun f -> "ErrorMessage", String.to_json f)
       ; Aws.Util.option_map v.error_code (fun f -> "ErrorCode", String.to_json f)
       ])

let of_json j =
  { error_code = Aws.Util.option_map (Aws.Json.lookup j "ErrorCode") String.of_json
  ; error_message = Aws.Util.option_map (Aws.Json.lookup j "ErrorMessage") String.of_json
  ; type_configuration_identifier =
      Aws.Util.option_map
        (Aws.Json.lookup j "TypeConfigurationIdentifier")
        TypeConfigurationIdentifier.of_json
  }
