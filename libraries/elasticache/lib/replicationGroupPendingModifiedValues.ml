open Aws.BaseTypes

type t =
  { primary_cluster_id : String.t option
  ; automatic_failover_status : PendingAutomaticFailoverStatus.t option
  ; resharding : ReshardingStatus.t option
  ; auth_token_status : AuthTokenUpdateStatus.t option
  ; user_groups : UserGroupsUpdateStatus.t option
  ; log_delivery_configurations : PendingLogDeliveryConfigurationList.t
  ; transit_encryption_enabled : Boolean.t option
  ; transit_encryption_mode : TransitEncryptionMode.t option
  ; cluster_mode : ClusterMode.t option
  }

let make
    ?primary_cluster_id
    ?automatic_failover_status
    ?resharding
    ?auth_token_status
    ?user_groups
    ?(log_delivery_configurations = [])
    ?transit_encryption_enabled
    ?transit_encryption_mode
    ?cluster_mode
    () =
  { primary_cluster_id
  ; automatic_failover_status
  ; resharding
  ; auth_token_status
  ; user_groups
  ; log_delivery_configurations
  ; transit_encryption_enabled
  ; transit_encryption_mode
  ; cluster_mode
  }

let parse xml =
  Some
    { primary_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "PrimaryClusterId" xml) String.parse
    ; automatic_failover_status =
        Aws.Util.option_bind
          (Aws.Xml.member "AutomaticFailoverStatus" xml)
          PendingAutomaticFailoverStatus.parse
    ; resharding =
        Aws.Util.option_bind (Aws.Xml.member "Resharding" xml) ReshardingStatus.parse
    ; auth_token_status =
        Aws.Util.option_bind
          (Aws.Xml.member "AuthTokenStatus" xml)
          AuthTokenUpdateStatus.parse
    ; user_groups =
        Aws.Util.option_bind
          (Aws.Xml.member "UserGroups" xml)
          UserGroupsUpdateStatus.parse
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
    ; cluster_mode =
        Aws.Util.option_bind (Aws.Xml.member "ClusterMode" xml) ClusterMode.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cluster_mode (fun f ->
             Aws.Query.Pair ("ClusterMode", ClusterMode.to_query f))
       ; Aws.Util.option_map v.transit_encryption_mode (fun f ->
             Aws.Query.Pair ("TransitEncryptionMode", TransitEncryptionMode.to_query f))
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             Aws.Query.Pair ("TransitEncryptionEnabled", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LogDeliveryConfigurations.member"
              , PendingLogDeliveryConfigurationList.to_query v.log_delivery_configurations
              ))
       ; Aws.Util.option_map v.user_groups (fun f ->
             Aws.Query.Pair ("UserGroups", UserGroupsUpdateStatus.to_query f))
       ; Aws.Util.option_map v.auth_token_status (fun f ->
             Aws.Query.Pair ("AuthTokenStatus", AuthTokenUpdateStatus.to_query f))
       ; Aws.Util.option_map v.resharding (fun f ->
             Aws.Query.Pair ("Resharding", ReshardingStatus.to_query f))
       ; Aws.Util.option_map v.automatic_failover_status (fun f ->
             Aws.Query.Pair
               ("AutomaticFailoverStatus", PendingAutomaticFailoverStatus.to_query f))
       ; Aws.Util.option_map v.primary_cluster_id (fun f ->
             Aws.Query.Pair ("PrimaryClusterId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cluster_mode (fun f ->
             "ClusterMode", ClusterMode.to_json f)
       ; Aws.Util.option_map v.transit_encryption_mode (fun f ->
             "TransitEncryptionMode", TransitEncryptionMode.to_json f)
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             "TransitEncryptionEnabled", Boolean.to_json f)
       ; Some
           ( "LogDeliveryConfigurations"
           , PendingLogDeliveryConfigurationList.to_json v.log_delivery_configurations )
       ; Aws.Util.option_map v.user_groups (fun f ->
             "UserGroups", UserGroupsUpdateStatus.to_json f)
       ; Aws.Util.option_map v.auth_token_status (fun f ->
             "AuthTokenStatus", AuthTokenUpdateStatus.to_json f)
       ; Aws.Util.option_map v.resharding (fun f ->
             "Resharding", ReshardingStatus.to_json f)
       ; Aws.Util.option_map v.automatic_failover_status (fun f ->
             "AutomaticFailoverStatus", PendingAutomaticFailoverStatus.to_json f)
       ; Aws.Util.option_map v.primary_cluster_id (fun f ->
             "PrimaryClusterId", String.to_json f)
       ])

let of_json j =
  { primary_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "PrimaryClusterId") String.of_json
  ; automatic_failover_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "AutomaticFailoverStatus")
        PendingAutomaticFailoverStatus.of_json
  ; resharding =
      Aws.Util.option_map (Aws.Json.lookup j "Resharding") ReshardingStatus.of_json
  ; auth_token_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "AuthTokenStatus")
        AuthTokenUpdateStatus.of_json
  ; user_groups =
      Aws.Util.option_map (Aws.Json.lookup j "UserGroups") UserGroupsUpdateStatus.of_json
  ; log_delivery_configurations =
      PendingLogDeliveryConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LogDeliveryConfigurations"))
  ; transit_encryption_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "TransitEncryptionEnabled") Boolean.of_json
  ; transit_encryption_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "TransitEncryptionMode")
        TransitEncryptionMode.of_json
  ; cluster_mode =
      Aws.Util.option_map (Aws.Json.lookup j "ClusterMode") ClusterMode.of_json
  }
