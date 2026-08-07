open Aws.BaseTypes

type t =
  { type_ : RegistryType.t option
  ; type_name : String.t
  ; schema_handler_package : String.t
  ; logging_config : LoggingConfig.t option
  ; execution_role_arn : String.t option
  ; client_request_token : String.t option
  }

let make
    ?type_
    ~type_name
    ~schema_handler_package
    ?logging_config
    ?execution_role_arn
    ?client_request_token
    () =
  { type_
  ; type_name
  ; schema_handler_package
  ; logging_config
  ; execution_role_arn
  ; client_request_token
  }

let parse xml =
  Some
    { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
    ; type_name =
        Aws.Xml.required
          "TypeName"
          (Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse)
    ; schema_handler_package =
        Aws.Xml.required
          "SchemaHandlerPackage"
          (Aws.Util.option_bind (Aws.Xml.member "SchemaHandlerPackage" xml) String.parse)
    ; logging_config =
        Aws.Util.option_bind (Aws.Xml.member "LoggingConfig" xml) LoggingConfig.parse
    ; execution_role_arn =
        Aws.Util.option_bind (Aws.Xml.member "ExecutionRoleArn" xml) String.parse
    ; client_request_token =
        Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_request_token (fun f ->
             Aws.Query.Pair ("ClientRequestToken", String.to_query f))
       ; Aws.Util.option_map v.execution_role_arn (fun f ->
             Aws.Query.Pair ("ExecutionRoleArn", String.to_query f))
       ; Aws.Util.option_map v.logging_config (fun f ->
             Aws.Query.Pair ("LoggingConfig", LoggingConfig.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SchemaHandlerPackage", String.to_query v.schema_handler_package))
       ; Some (Aws.Query.Pair ("TypeName", String.to_query v.type_name))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", RegistryType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_request_token (fun f ->
             "ClientRequestToken", String.to_json f)
       ; Aws.Util.option_map v.execution_role_arn (fun f ->
             "ExecutionRoleArn", String.to_json f)
       ; Aws.Util.option_map v.logging_config (fun f ->
             "LoggingConfig", LoggingConfig.to_json f)
       ; Some ("SchemaHandlerPackage", String.to_json v.schema_handler_package)
       ; Some ("TypeName", String.to_json v.type_name)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
       ])

let of_json j =
  { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
  ; type_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TypeName"))
  ; schema_handler_package =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SchemaHandlerPackage"))
  ; logging_config =
      Aws.Util.option_map (Aws.Json.lookup j "LoggingConfig") LoggingConfig.of_json
  ; execution_role_arn =
      Aws.Util.option_map (Aws.Json.lookup j "ExecutionRoleArn") String.of_json
  ; client_request_token =
      Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
  }
