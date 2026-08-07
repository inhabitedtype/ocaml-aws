open Aws.BaseTypes

type t =
  { num_cache_nodes : Integer.t option
  ; cache_node_ids_to_remove : CacheNodeIdsList.t
  ; engine_version : String.t option
  ; cache_node_type : String.t option
  ; auth_token_status : AuthTokenUpdateStatus.t option
  ; log_delivery_configurations : PendingLogDeliveryConfigurationList.t
  ; transit_encryption_enabled : Boolean.t option
  ; transit_encryption_mode : TransitEncryptionMode.t option
  ; scale_config : ScaleConfig.t option
  }

let make
    ?num_cache_nodes
    ?(cache_node_ids_to_remove = [])
    ?engine_version
    ?cache_node_type
    ?auth_token_status
    ?(log_delivery_configurations = [])
    ?transit_encryption_enabled
    ?transit_encryption_mode
    ?scale_config
    () =
  { num_cache_nodes
  ; cache_node_ids_to_remove
  ; engine_version
  ; cache_node_type
  ; auth_token_status
  ; log_delivery_configurations
  ; transit_encryption_enabled
  ; transit_encryption_mode
  ; scale_config
  }

let parse xml =
  Some
    { num_cache_nodes =
        Aws.Util.option_bind (Aws.Xml.member "NumCacheNodes" xml) Integer.parse
    ; cache_node_ids_to_remove =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheNodeIdsToRemove" xml)
             CacheNodeIdsList.parse)
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; cache_node_type =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
    ; auth_token_status =
        Aws.Util.option_bind
          (Aws.Xml.member "AuthTokenStatus" xml)
          AuthTokenUpdateStatus.parse
    ; log_delivery_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LogDeliveryConfigurations" xml)
             PendingLogDeliveryConfigurationList.parse)
    ; transit_encryption_enabled =
        Aws.Util.option_bind (Aws.Xml.member "TransitEncryptionEnabled" xml) Boolean.parse
    ; transit_encryption_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "TransitEncryptionMode" xml)
          TransitEncryptionMode.parse
    ; scale_config =
        Aws.Util.option_bind (Aws.Xml.member "ScaleConfig" xml) ScaleConfig.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scale_config (fun f ->
             Aws.Query.Pair ("ScaleConfig", ScaleConfig.to_query f))
       ; Aws.Util.option_map v.transit_encryption_mode (fun f ->
             Aws.Query.Pair ("TransitEncryptionMode", TransitEncryptionMode.to_query f))
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             Aws.Query.Pair ("TransitEncryptionEnabled", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LogDeliveryConfigurations.member"
              , PendingLogDeliveryConfigurationList.to_query v.log_delivery_configurations
              ))
       ; Aws.Util.option_map v.auth_token_status (fun f ->
             Aws.Query.Pair ("AuthTokenStatus", AuthTokenUpdateStatus.to_query f))
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             Aws.Query.Pair ("CacheNodeType", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CacheNodeIdsToRemove.member"
              , CacheNodeIdsList.to_query v.cache_node_ids_to_remove ))
       ; Aws.Util.option_map v.num_cache_nodes (fun f ->
             Aws.Query.Pair ("NumCacheNodes", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scale_config (fun f ->
             "ScaleConfig", ScaleConfig.to_json f)
       ; Aws.Util.option_map v.transit_encryption_mode (fun f ->
             "TransitEncryptionMode", TransitEncryptionMode.to_json f)
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             "TransitEncryptionEnabled", Boolean.to_json f)
       ; Some
           ( "LogDeliveryConfigurations"
           , PendingLogDeliveryConfigurationList.to_json v.log_delivery_configurations )
       ; Aws.Util.option_map v.auth_token_status (fun f ->
             "AuthTokenStatus", AuthTokenUpdateStatus.to_json f)
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             "CacheNodeType", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Some ("CacheNodeIdsToRemove", CacheNodeIdsList.to_json v.cache_node_ids_to_remove)
       ; Aws.Util.option_map v.num_cache_nodes (fun f ->
             "NumCacheNodes", Integer.to_json f)
       ])

let of_json j =
  { num_cache_nodes =
      Aws.Util.option_map (Aws.Json.lookup j "NumCacheNodes") Integer.of_json
  ; cache_node_ids_to_remove =
      CacheNodeIdsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodeIdsToRemove"))
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; cache_node_type =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
  ; auth_token_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "AuthTokenStatus")
        AuthTokenUpdateStatus.of_json
  ; log_delivery_configurations =
      PendingLogDeliveryConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LogDeliveryConfigurations"))
  ; transit_encryption_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "TransitEncryptionEnabled") Boolean.of_json
  ; transit_encryption_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "TransitEncryptionMode")
        TransitEncryptionMode.of_json
  ; scale_config =
      Aws.Util.option_map (Aws.Json.lookup j "ScaleConfig") ScaleConfig.of_json
  }
