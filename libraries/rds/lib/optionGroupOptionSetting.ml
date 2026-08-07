open Aws.BaseTypes

type t =
  { setting_name : String.t option
  ; setting_description : String.t option
  ; default_value : String.t option
  ; apply_type : String.t option
  ; allowed_values : String.t option
  ; is_modifiable : Boolean.t option
  ; is_required : Boolean.t option
  ; minimum_engine_version_per_allowed_value : MinimumEngineVersionPerAllowedValueList.t
  }

let make
    ?setting_name
    ?setting_description
    ?default_value
    ?apply_type
    ?allowed_values
    ?is_modifiable
    ?is_required
    ?(minimum_engine_version_per_allowed_value = [])
    () =
  { setting_name
  ; setting_description
  ; default_value
  ; apply_type
  ; allowed_values
  ; is_modifiable
  ; is_required
  ; minimum_engine_version_per_allowed_value
  }

let parse xml =
  Some
    { setting_name = Aws.Util.option_bind (Aws.Xml.member "SettingName" xml) String.parse
    ; setting_description =
        Aws.Util.option_bind (Aws.Xml.member "SettingDescription" xml) String.parse
    ; default_value =
        Aws.Util.option_bind (Aws.Xml.member "DefaultValue" xml) String.parse
    ; apply_type = Aws.Util.option_bind (Aws.Xml.member "ApplyType" xml) String.parse
    ; allowed_values =
        Aws.Util.option_bind (Aws.Xml.member "AllowedValues" xml) String.parse
    ; is_modifiable =
        Aws.Util.option_bind (Aws.Xml.member "IsModifiable" xml) Boolean.parse
    ; is_required = Aws.Util.option_bind (Aws.Xml.member "IsRequired" xml) Boolean.parse
    ; minimum_engine_version_per_allowed_value =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "MinimumEngineVersionPerAllowedValue" xml)
             MinimumEngineVersionPerAllowedValueList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "MinimumEngineVersionPerAllowedValue.member"
              , MinimumEngineVersionPerAllowedValueList.to_query
                  v.minimum_engine_version_per_allowed_value ))
       ; Aws.Util.option_map v.is_required (fun f ->
             Aws.Query.Pair ("IsRequired", Boolean.to_query f))
       ; Aws.Util.option_map v.is_modifiable (fun f ->
             Aws.Query.Pair ("IsModifiable", Boolean.to_query f))
       ; Aws.Util.option_map v.allowed_values (fun f ->
             Aws.Query.Pair ("AllowedValues", String.to_query f))
       ; Aws.Util.option_map v.apply_type (fun f ->
             Aws.Query.Pair ("ApplyType", String.to_query f))
       ; Aws.Util.option_map v.default_value (fun f ->
             Aws.Query.Pair ("DefaultValue", String.to_query f))
       ; Aws.Util.option_map v.setting_description (fun f ->
             Aws.Query.Pair ("SettingDescription", String.to_query f))
       ; Aws.Util.option_map v.setting_name (fun f ->
             Aws.Query.Pair ("SettingName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "MinimumEngineVersionPerAllowedValue"
           , MinimumEngineVersionPerAllowedValueList.to_json
               v.minimum_engine_version_per_allowed_value )
       ; Aws.Util.option_map v.is_required (fun f -> "IsRequired", Boolean.to_json f)
       ; Aws.Util.option_map v.is_modifiable (fun f -> "IsModifiable", Boolean.to_json f)
       ; Aws.Util.option_map v.allowed_values (fun f -> "AllowedValues", String.to_json f)
       ; Aws.Util.option_map v.apply_type (fun f -> "ApplyType", String.to_json f)
       ; Aws.Util.option_map v.default_value (fun f -> "DefaultValue", String.to_json f)
       ; Aws.Util.option_map v.setting_description (fun f ->
             "SettingDescription", String.to_json f)
       ; Aws.Util.option_map v.setting_name (fun f -> "SettingName", String.to_json f)
       ])

let of_json j =
  { setting_name = Aws.Util.option_map (Aws.Json.lookup j "SettingName") String.of_json
  ; setting_description =
      Aws.Util.option_map (Aws.Json.lookup j "SettingDescription") String.of_json
  ; default_value = Aws.Util.option_map (Aws.Json.lookup j "DefaultValue") String.of_json
  ; apply_type = Aws.Util.option_map (Aws.Json.lookup j "ApplyType") String.of_json
  ; allowed_values =
      Aws.Util.option_map (Aws.Json.lookup j "AllowedValues") String.of_json
  ; is_modifiable = Aws.Util.option_map (Aws.Json.lookup j "IsModifiable") Boolean.of_json
  ; is_required = Aws.Util.option_map (Aws.Json.lookup j "IsRequired") Boolean.of_json
  ; minimum_engine_version_per_allowed_value =
      MinimumEngineVersionPerAllowedValueList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MinimumEngineVersionPerAllowedValue"))
  }
