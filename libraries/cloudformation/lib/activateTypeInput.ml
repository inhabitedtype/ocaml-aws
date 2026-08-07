open Aws.BaseTypes

type t =
  { type_ : ThirdPartyType.t option
  ; public_type_arn : String.t option
  ; publisher_id : String.t option
  ; type_name : String.t option
  ; type_name_alias : String.t option
  ; auto_update : Boolean.t option
  ; logging_config : LoggingConfig.t option
  ; execution_role_arn : String.t option
  ; version_bump : VersionBump.t option
  ; major_version : Long.t option
  }

let make
    ?type_
    ?public_type_arn
    ?publisher_id
    ?type_name
    ?type_name_alias
    ?auto_update
    ?logging_config
    ?execution_role_arn
    ?version_bump
    ?major_version
    () =
  { type_
  ; public_type_arn
  ; publisher_id
  ; type_name
  ; type_name_alias
  ; auto_update
  ; logging_config
  ; execution_role_arn
  ; version_bump
  ; major_version
  }

let parse xml =
  Some
    { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) ThirdPartyType.parse
    ; public_type_arn =
        Aws.Util.option_bind (Aws.Xml.member "PublicTypeArn" xml) String.parse
    ; publisher_id = Aws.Util.option_bind (Aws.Xml.member "PublisherId" xml) String.parse
    ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
    ; type_name_alias =
        Aws.Util.option_bind (Aws.Xml.member "TypeNameAlias" xml) String.parse
    ; auto_update = Aws.Util.option_bind (Aws.Xml.member "AutoUpdate" xml) Boolean.parse
    ; logging_config =
        Aws.Util.option_bind (Aws.Xml.member "LoggingConfig" xml) LoggingConfig.parse
    ; execution_role_arn =
        Aws.Util.option_bind (Aws.Xml.member "ExecutionRoleArn" xml) String.parse
    ; version_bump =
        Aws.Util.option_bind (Aws.Xml.member "VersionBump" xml) VersionBump.parse
    ; major_version = Aws.Util.option_bind (Aws.Xml.member "MajorVersion" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.major_version (fun f ->
             Aws.Query.Pair ("MajorVersion", Long.to_query f))
       ; Aws.Util.option_map v.version_bump (fun f ->
             Aws.Query.Pair ("VersionBump", VersionBump.to_query f))
       ; Aws.Util.option_map v.execution_role_arn (fun f ->
             Aws.Query.Pair ("ExecutionRoleArn", String.to_query f))
       ; Aws.Util.option_map v.logging_config (fun f ->
             Aws.Query.Pair ("LoggingConfig", LoggingConfig.to_query f))
       ; Aws.Util.option_map v.auto_update (fun f ->
             Aws.Query.Pair ("AutoUpdate", Boolean.to_query f))
       ; Aws.Util.option_map v.type_name_alias (fun f ->
             Aws.Query.Pair ("TypeNameAlias", String.to_query f))
       ; Aws.Util.option_map v.type_name (fun f ->
             Aws.Query.Pair ("TypeName", String.to_query f))
       ; Aws.Util.option_map v.publisher_id (fun f ->
             Aws.Query.Pair ("PublisherId", String.to_query f))
       ; Aws.Util.option_map v.public_type_arn (fun f ->
             Aws.Query.Pair ("PublicTypeArn", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", ThirdPartyType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.major_version (fun f -> "MajorVersion", Long.to_json f)
       ; Aws.Util.option_map v.version_bump (fun f ->
             "VersionBump", VersionBump.to_json f)
       ; Aws.Util.option_map v.execution_role_arn (fun f ->
             "ExecutionRoleArn", String.to_json f)
       ; Aws.Util.option_map v.logging_config (fun f ->
             "LoggingConfig", LoggingConfig.to_json f)
       ; Aws.Util.option_map v.auto_update (fun f -> "AutoUpdate", Boolean.to_json f)
       ; Aws.Util.option_map v.type_name_alias (fun f ->
             "TypeNameAlias", String.to_json f)
       ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
       ; Aws.Util.option_map v.publisher_id (fun f -> "PublisherId", String.to_json f)
       ; Aws.Util.option_map v.public_type_arn (fun f ->
             "PublicTypeArn", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", ThirdPartyType.to_json f)
       ])

let of_json j =
  { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") ThirdPartyType.of_json
  ; public_type_arn =
      Aws.Util.option_map (Aws.Json.lookup j "PublicTypeArn") String.of_json
  ; publisher_id = Aws.Util.option_map (Aws.Json.lookup j "PublisherId") String.of_json
  ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
  ; type_name_alias =
      Aws.Util.option_map (Aws.Json.lookup j "TypeNameAlias") String.of_json
  ; auto_update = Aws.Util.option_map (Aws.Json.lookup j "AutoUpdate") Boolean.of_json
  ; logging_config =
      Aws.Util.option_map (Aws.Json.lookup j "LoggingConfig") LoggingConfig.of_json
  ; execution_role_arn =
      Aws.Util.option_map (Aws.Json.lookup j "ExecutionRoleArn") String.of_json
  ; version_bump =
      Aws.Util.option_map (Aws.Json.lookup j "VersionBump") VersionBump.of_json
  ; major_version = Aws.Util.option_map (Aws.Json.lookup j "MajorVersion") Long.of_json
  }
