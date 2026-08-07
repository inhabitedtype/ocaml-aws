open Aws.BaseTypes

type t =
  { arn : String.t option
  ; type_ : RegistryType.t option
  ; type_name : String.t option
  ; default_version_id : String.t option
  ; is_default_version : Boolean.t option
  ; type_tests_status : TypeTestsStatus.t option
  ; type_tests_status_description : String.t option
  ; description : String.t option
  ; schema : String.t option
  ; provisioning_type : ProvisioningType.t option
  ; deprecated_status : DeprecatedStatus.t option
  ; logging_config : LoggingConfig.t option
  ; required_activated_types : RequiredActivatedTypes.t
  ; execution_role_arn : String.t option
  ; visibility : Visibility.t option
  ; source_url : String.t option
  ; documentation_url : String.t option
  ; last_updated : DateTime.t option
  ; time_created : DateTime.t option
  ; configuration_schema : String.t option
  ; publisher_id : String.t option
  ; original_type_name : String.t option
  ; original_type_arn : String.t option
  ; public_version_number : String.t option
  ; latest_public_version : String.t option
  ; is_activated : Boolean.t option
  ; auto_update : Boolean.t option
  }

let make
    ?arn
    ?type_
    ?type_name
    ?default_version_id
    ?is_default_version
    ?type_tests_status
    ?type_tests_status_description
    ?description
    ?schema
    ?provisioning_type
    ?deprecated_status
    ?logging_config
    ?(required_activated_types = [])
    ?execution_role_arn
    ?visibility
    ?source_url
    ?documentation_url
    ?last_updated
    ?time_created
    ?configuration_schema
    ?publisher_id
    ?original_type_name
    ?original_type_arn
    ?public_version_number
    ?latest_public_version
    ?is_activated
    ?auto_update
    () =
  { arn
  ; type_
  ; type_name
  ; default_version_id
  ; is_default_version
  ; type_tests_status
  ; type_tests_status_description
  ; description
  ; schema
  ; provisioning_type
  ; deprecated_status
  ; logging_config
  ; required_activated_types
  ; execution_role_arn
  ; visibility
  ; source_url
  ; documentation_url
  ; last_updated
  ; time_created
  ; configuration_schema
  ; publisher_id
  ; original_type_name
  ; original_type_arn
  ; public_version_number
  ; latest_public_version
  ; is_activated
  ; auto_update
  }

let parse xml =
  Some
    { arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
    ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
    ; default_version_id =
        Aws.Util.option_bind (Aws.Xml.member "DefaultVersionId" xml) String.parse
    ; is_default_version =
        Aws.Util.option_bind (Aws.Xml.member "IsDefaultVersion" xml) Boolean.parse
    ; type_tests_status =
        Aws.Util.option_bind (Aws.Xml.member "TypeTestsStatus" xml) TypeTestsStatus.parse
    ; type_tests_status_description =
        Aws.Util.option_bind
          (Aws.Xml.member "TypeTestsStatusDescription" xml)
          String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; schema = Aws.Util.option_bind (Aws.Xml.member "Schema" xml) String.parse
    ; provisioning_type =
        Aws.Util.option_bind
          (Aws.Xml.member "ProvisioningType" xml)
          ProvisioningType.parse
    ; deprecated_status =
        Aws.Util.option_bind
          (Aws.Xml.member "DeprecatedStatus" xml)
          DeprecatedStatus.parse
    ; logging_config =
        Aws.Util.option_bind (Aws.Xml.member "LoggingConfig" xml) LoggingConfig.parse
    ; required_activated_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RequiredActivatedTypes" xml)
             RequiredActivatedTypes.parse)
    ; execution_role_arn =
        Aws.Util.option_bind (Aws.Xml.member "ExecutionRoleArn" xml) String.parse
    ; visibility = Aws.Util.option_bind (Aws.Xml.member "Visibility" xml) Visibility.parse
    ; source_url = Aws.Util.option_bind (Aws.Xml.member "SourceUrl" xml) String.parse
    ; documentation_url =
        Aws.Util.option_bind (Aws.Xml.member "DocumentationUrl" xml) String.parse
    ; last_updated =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdated" xml) DateTime.parse
    ; time_created =
        Aws.Util.option_bind (Aws.Xml.member "TimeCreated" xml) DateTime.parse
    ; configuration_schema =
        Aws.Util.option_bind (Aws.Xml.member "ConfigurationSchema" xml) String.parse
    ; publisher_id = Aws.Util.option_bind (Aws.Xml.member "PublisherId" xml) String.parse
    ; original_type_name =
        Aws.Util.option_bind (Aws.Xml.member "OriginalTypeName" xml) String.parse
    ; original_type_arn =
        Aws.Util.option_bind (Aws.Xml.member "OriginalTypeArn" xml) String.parse
    ; public_version_number =
        Aws.Util.option_bind (Aws.Xml.member "PublicVersionNumber" xml) String.parse
    ; latest_public_version =
        Aws.Util.option_bind (Aws.Xml.member "LatestPublicVersion" xml) String.parse
    ; is_activated = Aws.Util.option_bind (Aws.Xml.member "IsActivated" xml) Boolean.parse
    ; auto_update = Aws.Util.option_bind (Aws.Xml.member "AutoUpdate" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.auto_update (fun f ->
             Aws.Query.Pair ("AutoUpdate", Boolean.to_query f))
       ; Aws.Util.option_map v.is_activated (fun f ->
             Aws.Query.Pair ("IsActivated", Boolean.to_query f))
       ; Aws.Util.option_map v.latest_public_version (fun f ->
             Aws.Query.Pair ("LatestPublicVersion", String.to_query f))
       ; Aws.Util.option_map v.public_version_number (fun f ->
             Aws.Query.Pair ("PublicVersionNumber", String.to_query f))
       ; Aws.Util.option_map v.original_type_arn (fun f ->
             Aws.Query.Pair ("OriginalTypeArn", String.to_query f))
       ; Aws.Util.option_map v.original_type_name (fun f ->
             Aws.Query.Pair ("OriginalTypeName", String.to_query f))
       ; Aws.Util.option_map v.publisher_id (fun f ->
             Aws.Query.Pair ("PublisherId", String.to_query f))
       ; Aws.Util.option_map v.configuration_schema (fun f ->
             Aws.Query.Pair ("ConfigurationSchema", String.to_query f))
       ; Aws.Util.option_map v.time_created (fun f ->
             Aws.Query.Pair ("TimeCreated", DateTime.to_query f))
       ; Aws.Util.option_map v.last_updated (fun f ->
             Aws.Query.Pair ("LastUpdated", DateTime.to_query f))
       ; Aws.Util.option_map v.documentation_url (fun f ->
             Aws.Query.Pair ("DocumentationUrl", String.to_query f))
       ; Aws.Util.option_map v.source_url (fun f ->
             Aws.Query.Pair ("SourceUrl", String.to_query f))
       ; Aws.Util.option_map v.visibility (fun f ->
             Aws.Query.Pair ("Visibility", Visibility.to_query f))
       ; Aws.Util.option_map v.execution_role_arn (fun f ->
             Aws.Query.Pair ("ExecutionRoleArn", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "RequiredActivatedTypes.member"
              , RequiredActivatedTypes.to_query v.required_activated_types ))
       ; Aws.Util.option_map v.logging_config (fun f ->
             Aws.Query.Pair ("LoggingConfig", LoggingConfig.to_query f))
       ; Aws.Util.option_map v.deprecated_status (fun f ->
             Aws.Query.Pair ("DeprecatedStatus", DeprecatedStatus.to_query f))
       ; Aws.Util.option_map v.provisioning_type (fun f ->
             Aws.Query.Pair ("ProvisioningType", ProvisioningType.to_query f))
       ; Aws.Util.option_map v.schema (fun f ->
             Aws.Query.Pair ("Schema", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.type_tests_status_description (fun f ->
             Aws.Query.Pair ("TypeTestsStatusDescription", String.to_query f))
       ; Aws.Util.option_map v.type_tests_status (fun f ->
             Aws.Query.Pair ("TypeTestsStatus", TypeTestsStatus.to_query f))
       ; Aws.Util.option_map v.is_default_version (fun f ->
             Aws.Query.Pair ("IsDefaultVersion", Boolean.to_query f))
       ; Aws.Util.option_map v.default_version_id (fun f ->
             Aws.Query.Pair ("DefaultVersionId", String.to_query f))
       ; Aws.Util.option_map v.type_name (fun f ->
             Aws.Query.Pair ("TypeName", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", RegistryType.to_query f))
       ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.auto_update (fun f -> "AutoUpdate", Boolean.to_json f)
       ; Aws.Util.option_map v.is_activated (fun f -> "IsActivated", Boolean.to_json f)
       ; Aws.Util.option_map v.latest_public_version (fun f ->
             "LatestPublicVersion", String.to_json f)
       ; Aws.Util.option_map v.public_version_number (fun f ->
             "PublicVersionNumber", String.to_json f)
       ; Aws.Util.option_map v.original_type_arn (fun f ->
             "OriginalTypeArn", String.to_json f)
       ; Aws.Util.option_map v.original_type_name (fun f ->
             "OriginalTypeName", String.to_json f)
       ; Aws.Util.option_map v.publisher_id (fun f -> "PublisherId", String.to_json f)
       ; Aws.Util.option_map v.configuration_schema (fun f ->
             "ConfigurationSchema", String.to_json f)
       ; Aws.Util.option_map v.time_created (fun f -> "TimeCreated", DateTime.to_json f)
       ; Aws.Util.option_map v.last_updated (fun f -> "LastUpdated", DateTime.to_json f)
       ; Aws.Util.option_map v.documentation_url (fun f ->
             "DocumentationUrl", String.to_json f)
       ; Aws.Util.option_map v.source_url (fun f -> "SourceUrl", String.to_json f)
       ; Aws.Util.option_map v.visibility (fun f -> "Visibility", Visibility.to_json f)
       ; Aws.Util.option_map v.execution_role_arn (fun f ->
             "ExecutionRoleArn", String.to_json f)
       ; Some
           ( "RequiredActivatedTypes"
           , RequiredActivatedTypes.to_json v.required_activated_types )
       ; Aws.Util.option_map v.logging_config (fun f ->
             "LoggingConfig", LoggingConfig.to_json f)
       ; Aws.Util.option_map v.deprecated_status (fun f ->
             "DeprecatedStatus", DeprecatedStatus.to_json f)
       ; Aws.Util.option_map v.provisioning_type (fun f ->
             "ProvisioningType", ProvisioningType.to_json f)
       ; Aws.Util.option_map v.schema (fun f -> "Schema", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.type_tests_status_description (fun f ->
             "TypeTestsStatusDescription", String.to_json f)
       ; Aws.Util.option_map v.type_tests_status (fun f ->
             "TypeTestsStatus", TypeTestsStatus.to_json f)
       ; Aws.Util.option_map v.is_default_version (fun f ->
             "IsDefaultVersion", Boolean.to_json f)
       ; Aws.Util.option_map v.default_version_id (fun f ->
             "DefaultVersionId", String.to_json f)
       ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
       ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
       ])

let of_json j =
  { arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
  ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
  ; default_version_id =
      Aws.Util.option_map (Aws.Json.lookup j "DefaultVersionId") String.of_json
  ; is_default_version =
      Aws.Util.option_map (Aws.Json.lookup j "IsDefaultVersion") Boolean.of_json
  ; type_tests_status =
      Aws.Util.option_map (Aws.Json.lookup j "TypeTestsStatus") TypeTestsStatus.of_json
  ; type_tests_status_description =
      Aws.Util.option_map (Aws.Json.lookup j "TypeTestsStatusDescription") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; schema = Aws.Util.option_map (Aws.Json.lookup j "Schema") String.of_json
  ; provisioning_type =
      Aws.Util.option_map (Aws.Json.lookup j "ProvisioningType") ProvisioningType.of_json
  ; deprecated_status =
      Aws.Util.option_map (Aws.Json.lookup j "DeprecatedStatus") DeprecatedStatus.of_json
  ; logging_config =
      Aws.Util.option_map (Aws.Json.lookup j "LoggingConfig") LoggingConfig.of_json
  ; required_activated_types =
      RequiredActivatedTypes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RequiredActivatedTypes"))
  ; execution_role_arn =
      Aws.Util.option_map (Aws.Json.lookup j "ExecutionRoleArn") String.of_json
  ; visibility = Aws.Util.option_map (Aws.Json.lookup j "Visibility") Visibility.of_json
  ; source_url = Aws.Util.option_map (Aws.Json.lookup j "SourceUrl") String.of_json
  ; documentation_url =
      Aws.Util.option_map (Aws.Json.lookup j "DocumentationUrl") String.of_json
  ; last_updated = Aws.Util.option_map (Aws.Json.lookup j "LastUpdated") DateTime.of_json
  ; time_created = Aws.Util.option_map (Aws.Json.lookup j "TimeCreated") DateTime.of_json
  ; configuration_schema =
      Aws.Util.option_map (Aws.Json.lookup j "ConfigurationSchema") String.of_json
  ; publisher_id = Aws.Util.option_map (Aws.Json.lookup j "PublisherId") String.of_json
  ; original_type_name =
      Aws.Util.option_map (Aws.Json.lookup j "OriginalTypeName") String.of_json
  ; original_type_arn =
      Aws.Util.option_map (Aws.Json.lookup j "OriginalTypeArn") String.of_json
  ; public_version_number =
      Aws.Util.option_map (Aws.Json.lookup j "PublicVersionNumber") String.of_json
  ; latest_public_version =
      Aws.Util.option_map (Aws.Json.lookup j "LatestPublicVersion") String.of_json
  ; is_activated = Aws.Util.option_map (Aws.Json.lookup j "IsActivated") Boolean.of_json
  ; auto_update = Aws.Util.option_map (Aws.Json.lookup j "AutoUpdate") Boolean.of_json
  }
