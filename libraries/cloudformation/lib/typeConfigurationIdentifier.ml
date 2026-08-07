open Aws.BaseTypes

type t =
  { type_arn : String.t option
  ; type_configuration_alias : String.t option
  ; type_configuration_arn : String.t option
  ; type_ : ThirdPartyType.t option
  ; type_name : String.t option
  }

let make ?type_arn ?type_configuration_alias ?type_configuration_arn ?type_ ?type_name ()
    =
  { type_arn; type_configuration_alias; type_configuration_arn; type_; type_name }

let parse xml =
  Some
    { type_arn = Aws.Util.option_bind (Aws.Xml.member "TypeArn" xml) String.parse
    ; type_configuration_alias =
        Aws.Util.option_bind (Aws.Xml.member "TypeConfigurationAlias" xml) String.parse
    ; type_configuration_arn =
        Aws.Util.option_bind (Aws.Xml.member "TypeConfigurationArn" xml) String.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) ThirdPartyType.parse
    ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_name (fun f ->
             Aws.Query.Pair ("TypeName", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", ThirdPartyType.to_query f))
       ; Aws.Util.option_map v.type_configuration_arn (fun f ->
             Aws.Query.Pair ("TypeConfigurationArn", String.to_query f))
       ; Aws.Util.option_map v.type_configuration_alias (fun f ->
             Aws.Query.Pair ("TypeConfigurationAlias", String.to_query f))
       ; Aws.Util.option_map v.type_arn (fun f ->
             Aws.Query.Pair ("TypeArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", ThirdPartyType.to_json f)
       ; Aws.Util.option_map v.type_configuration_arn (fun f ->
             "TypeConfigurationArn", String.to_json f)
       ; Aws.Util.option_map v.type_configuration_alias (fun f ->
             "TypeConfigurationAlias", String.to_json f)
       ; Aws.Util.option_map v.type_arn (fun f -> "TypeArn", String.to_json f)
       ])

let of_json j =
  { type_arn = Aws.Util.option_map (Aws.Json.lookup j "TypeArn") String.of_json
  ; type_configuration_alias =
      Aws.Util.option_map (Aws.Json.lookup j "TypeConfigurationAlias") String.of_json
  ; type_configuration_arn =
      Aws.Util.option_map (Aws.Json.lookup j "TypeConfigurationArn") String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") ThirdPartyType.of_json
  ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
  }
