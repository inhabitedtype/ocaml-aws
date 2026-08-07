open Aws.BaseTypes

type t =
  { engine : String.t option
  ; engine_version : String.t option
  ; description : String.t option
  ; auto_upgrade : Boolean.t option
  ; is_major_version_upgrade : Boolean.t option
  ; supported_engine_modes : EngineModeList.t
  ; supports_parallel_query : Boolean.t option
  ; supports_global_databases : Boolean.t option
  ; supports_babelfish : Boolean.t option
  ; supports_limitless_database : Boolean.t option
  ; supports_local_write_forwarding : Boolean.t option
  ; supports_integrations : Boolean.t option
  }

let make
    ?engine
    ?engine_version
    ?description
    ?auto_upgrade
    ?is_major_version_upgrade
    ?(supported_engine_modes = [])
    ?supports_parallel_query
    ?supports_global_databases
    ?supports_babelfish
    ?supports_limitless_database
    ?supports_local_write_forwarding
    ?supports_integrations
    () =
  { engine
  ; engine_version
  ; description
  ; auto_upgrade
  ; is_major_version_upgrade
  ; supported_engine_modes
  ; supports_parallel_query
  ; supports_global_databases
  ; supports_babelfish
  ; supports_limitless_database
  ; supports_local_write_forwarding
  ; supports_integrations
  }

let parse xml =
  Some
    { engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; auto_upgrade = Aws.Util.option_bind (Aws.Xml.member "AutoUpgrade" xml) Boolean.parse
    ; is_major_version_upgrade =
        Aws.Util.option_bind (Aws.Xml.member "IsMajorVersionUpgrade" xml) Boolean.parse
    ; supported_engine_modes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedEngineModes" xml)
             EngineModeList.parse)
    ; supports_parallel_query =
        Aws.Util.option_bind (Aws.Xml.member "SupportsParallelQuery" xml) Boolean.parse
    ; supports_global_databases =
        Aws.Util.option_bind (Aws.Xml.member "SupportsGlobalDatabases" xml) Boolean.parse
    ; supports_babelfish =
        Aws.Util.option_bind (Aws.Xml.member "SupportsBabelfish" xml) Boolean.parse
    ; supports_limitless_database =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsLimitlessDatabase" xml)
          Boolean.parse
    ; supports_local_write_forwarding =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsLocalWriteForwarding" xml)
          Boolean.parse
    ; supports_integrations =
        Aws.Util.option_bind (Aws.Xml.member "SupportsIntegrations" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.supports_integrations (fun f ->
             Aws.Query.Pair ("SupportsIntegrations", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_local_write_forwarding (fun f ->
             Aws.Query.Pair ("SupportsLocalWriteForwarding", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_limitless_database (fun f ->
             Aws.Query.Pair ("SupportsLimitlessDatabase", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_babelfish (fun f ->
             Aws.Query.Pair ("SupportsBabelfish", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_global_databases (fun f ->
             Aws.Query.Pair ("SupportsGlobalDatabases", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_parallel_query (fun f ->
             Aws.Query.Pair ("SupportsParallelQuery", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SupportedEngineModes.member"
              , EngineModeList.to_query v.supported_engine_modes ))
       ; Aws.Util.option_map v.is_major_version_upgrade (fun f ->
             Aws.Query.Pair ("IsMajorVersionUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.auto_upgrade (fun f ->
             Aws.Query.Pair ("AutoUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.supports_integrations (fun f ->
             "SupportsIntegrations", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_local_write_forwarding (fun f ->
             "SupportsLocalWriteForwarding", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_limitless_database (fun f ->
             "SupportsLimitlessDatabase", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_babelfish (fun f ->
             "SupportsBabelfish", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_global_databases (fun f ->
             "SupportsGlobalDatabases", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_parallel_query (fun f ->
             "SupportsParallelQuery", Boolean.to_json f)
       ; Some ("SupportedEngineModes", EngineModeList.to_json v.supported_engine_modes)
       ; Aws.Util.option_map v.is_major_version_upgrade (fun f ->
             "IsMajorVersionUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.auto_upgrade (fun f -> "AutoUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ])

let of_json j =
  { engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; auto_upgrade = Aws.Util.option_map (Aws.Json.lookup j "AutoUpgrade") Boolean.of_json
  ; is_major_version_upgrade =
      Aws.Util.option_map (Aws.Json.lookup j "IsMajorVersionUpgrade") Boolean.of_json
  ; supported_engine_modes =
      EngineModeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedEngineModes"))
  ; supports_parallel_query =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsParallelQuery") Boolean.of_json
  ; supports_global_databases =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsGlobalDatabases") Boolean.of_json
  ; supports_babelfish =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsBabelfish") Boolean.of_json
  ; supports_limitless_database =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsLimitlessDatabase") Boolean.of_json
  ; supports_local_write_forwarding =
      Aws.Util.option_map
        (Aws.Json.lookup j "SupportsLocalWriteForwarding")
        Boolean.of_json
  ; supports_integrations =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsIntegrations") Boolean.of_json
  }
