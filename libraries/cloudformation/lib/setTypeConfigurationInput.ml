open Aws.BaseTypes

type t =
  { type_arn : String.t option
  ; configuration : String.t
  ; configuration_alias : String.t option
  ; type_name : String.t option
  ; type_ : ThirdPartyType.t option
  }

let make ?type_arn ~configuration ?configuration_alias ?type_name ?type_ () =
  { type_arn; configuration; configuration_alias; type_name; type_ }

let parse xml =
  Some
    { type_arn = Aws.Util.option_bind (Aws.Xml.member "TypeArn" xml) String.parse
    ; configuration =
        Aws.Xml.required
          "Configuration"
          (Aws.Util.option_bind (Aws.Xml.member "Configuration" xml) String.parse)
    ; configuration_alias =
        Aws.Util.option_bind (Aws.Xml.member "ConfigurationAlias" xml) String.parse
    ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) ThirdPartyType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", ThirdPartyType.to_query f))
       ; Aws.Util.option_map v.type_name (fun f ->
             Aws.Query.Pair ("TypeName", String.to_query f))
       ; Aws.Util.option_map v.configuration_alias (fun f ->
             Aws.Query.Pair ("ConfigurationAlias", String.to_query f))
       ; Some (Aws.Query.Pair ("Configuration", String.to_query v.configuration))
       ; Aws.Util.option_map v.type_arn (fun f ->
             Aws.Query.Pair ("TypeArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_ (fun f -> "Type", ThirdPartyType.to_json f)
       ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
       ; Aws.Util.option_map v.configuration_alias (fun f ->
             "ConfigurationAlias", String.to_json f)
       ; Some ("Configuration", String.to_json v.configuration)
       ; Aws.Util.option_map v.type_arn (fun f -> "TypeArn", String.to_json f)
       ])

let of_json j =
  { type_arn = Aws.Util.option_map (Aws.Json.lookup j "TypeArn") String.of_json
  ; configuration =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Configuration"))
  ; configuration_alias =
      Aws.Util.option_map (Aws.Json.lookup j "ConfigurationAlias") String.of_json
  ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") ThirdPartyType.of_json
  }
