open Aws.BaseTypes

type t =
  { replication_group_id : String.t option
  ; cache_cluster_id : String.t option
  ; service_update_name : String.t option
  ; service_update_release_date : DateTime.t option
  ; service_update_severity : ServiceUpdateSeverity.t option
  ; service_update_status : ServiceUpdateStatus.t option
  ; service_update_recommended_apply_by_date : DateTime.t option
  ; service_update_type : ServiceUpdateType.t option
  ; update_action_available_date : DateTime.t option
  ; update_action_status : UpdateActionStatus.t option
  ; nodes_updated : String.t option
  ; update_action_status_modified_date : DateTime.t option
  ; sla_met : SlaMet.t option
  ; node_group_update_status : NodeGroupUpdateStatusList.t
  ; cache_node_update_status : CacheNodeUpdateStatusList.t
  ; estimated_update_time : String.t option
  ; engine : String.t option
  }

let make
    ?replication_group_id
    ?cache_cluster_id
    ?service_update_name
    ?service_update_release_date
    ?service_update_severity
    ?service_update_status
    ?service_update_recommended_apply_by_date
    ?service_update_type
    ?update_action_available_date
    ?update_action_status
    ?nodes_updated
    ?update_action_status_modified_date
    ?sla_met
    ?(node_group_update_status = [])
    ?(cache_node_update_status = [])
    ?estimated_update_time
    ?engine
    () =
  { replication_group_id
  ; cache_cluster_id
  ; service_update_name
  ; service_update_release_date
  ; service_update_severity
  ; service_update_status
  ; service_update_recommended_apply_by_date
  ; service_update_type
  ; update_action_available_date
  ; update_action_status
  ; nodes_updated
  ; update_action_status_modified_date
  ; sla_met
  ; node_group_update_status
  ; cache_node_update_status
  ; estimated_update_time
  ; engine
  }

let parse xml =
  Some
    { replication_group_id =
        Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
    ; cache_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
    ; service_update_name =
        Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateName" xml) String.parse
    ; service_update_release_date =
        Aws.Util.option_bind
          (Aws.Xml.member "ServiceUpdateReleaseDate" xml)
          DateTime.parse
    ; service_update_severity =
        Aws.Util.option_bind
          (Aws.Xml.member "ServiceUpdateSeverity" xml)
          ServiceUpdateSeverity.parse
    ; service_update_status =
        Aws.Util.option_bind
          (Aws.Xml.member "ServiceUpdateStatus" xml)
          ServiceUpdateStatus.parse
    ; service_update_recommended_apply_by_date =
        Aws.Util.option_bind
          (Aws.Xml.member "ServiceUpdateRecommendedApplyByDate" xml)
          DateTime.parse
    ; service_update_type =
        Aws.Util.option_bind
          (Aws.Xml.member "ServiceUpdateType" xml)
          ServiceUpdateType.parse
    ; update_action_available_date =
        Aws.Util.option_bind
          (Aws.Xml.member "UpdateActionAvailableDate" xml)
          DateTime.parse
    ; update_action_status =
        Aws.Util.option_bind
          (Aws.Xml.member "UpdateActionStatus" xml)
          UpdateActionStatus.parse
    ; nodes_updated =
        Aws.Util.option_bind (Aws.Xml.member "NodesUpdated" xml) String.parse
    ; update_action_status_modified_date =
        Aws.Util.option_bind
          (Aws.Xml.member "UpdateActionStatusModifiedDate" xml)
          DateTime.parse
    ; sla_met = Aws.Util.option_bind (Aws.Xml.member "SlaMet" xml) SlaMet.parse
    ; node_group_update_status =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NodeGroupUpdateStatus" xml)
             NodeGroupUpdateStatusList.parse)
    ; cache_node_update_status =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheNodeUpdateStatus" xml)
             CacheNodeUpdateStatusList.parse)
    ; estimated_update_time =
        Aws.Util.option_bind (Aws.Xml.member "EstimatedUpdateTime" xml) String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.estimated_update_time (fun f ->
             Aws.Query.Pair ("EstimatedUpdateTime", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CacheNodeUpdateStatus.member"
              , CacheNodeUpdateStatusList.to_query v.cache_node_update_status ))
       ; Some
           (Aws.Query.Pair
              ( "NodeGroupUpdateStatus.member"
              , NodeGroupUpdateStatusList.to_query v.node_group_update_status ))
       ; Aws.Util.option_map v.sla_met (fun f ->
             Aws.Query.Pair ("SlaMet", SlaMet.to_query f))
       ; Aws.Util.option_map v.update_action_status_modified_date (fun f ->
             Aws.Query.Pair ("UpdateActionStatusModifiedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.nodes_updated (fun f ->
             Aws.Query.Pair ("NodesUpdated", String.to_query f))
       ; Aws.Util.option_map v.update_action_status (fun f ->
             Aws.Query.Pair ("UpdateActionStatus", UpdateActionStatus.to_query f))
       ; Aws.Util.option_map v.update_action_available_date (fun f ->
             Aws.Query.Pair ("UpdateActionAvailableDate", DateTime.to_query f))
       ; Aws.Util.option_map v.service_update_type (fun f ->
             Aws.Query.Pair ("ServiceUpdateType", ServiceUpdateType.to_query f))
       ; Aws.Util.option_map v.service_update_recommended_apply_by_date (fun f ->
             Aws.Query.Pair ("ServiceUpdateRecommendedApplyByDate", DateTime.to_query f))
       ; Aws.Util.option_map v.service_update_status (fun f ->
             Aws.Query.Pair ("ServiceUpdateStatus", ServiceUpdateStatus.to_query f))
       ; Aws.Util.option_map v.service_update_severity (fun f ->
             Aws.Query.Pair ("ServiceUpdateSeverity", ServiceUpdateSeverity.to_query f))
       ; Aws.Util.option_map v.service_update_release_date (fun f ->
             Aws.Query.Pair ("ServiceUpdateReleaseDate", DateTime.to_query f))
       ; Aws.Util.option_map v.service_update_name (fun f ->
             Aws.Query.Pair ("ServiceUpdateName", String.to_query f))
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             Aws.Query.Pair ("CacheClusterId", String.to_query f))
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.estimated_update_time (fun f ->
             "EstimatedUpdateTime", String.to_json f)
       ; Some
           ( "CacheNodeUpdateStatus"
           , CacheNodeUpdateStatusList.to_json v.cache_node_update_status )
       ; Some
           ( "NodeGroupUpdateStatus"
           , NodeGroupUpdateStatusList.to_json v.node_group_update_status )
       ; Aws.Util.option_map v.sla_met (fun f -> "SlaMet", SlaMet.to_json f)
       ; Aws.Util.option_map v.update_action_status_modified_date (fun f ->
             "UpdateActionStatusModifiedDate", DateTime.to_json f)
       ; Aws.Util.option_map v.nodes_updated (fun f -> "NodesUpdated", String.to_json f)
       ; Aws.Util.option_map v.update_action_status (fun f ->
             "UpdateActionStatus", UpdateActionStatus.to_json f)
       ; Aws.Util.option_map v.update_action_available_date (fun f ->
             "UpdateActionAvailableDate", DateTime.to_json f)
       ; Aws.Util.option_map v.service_update_type (fun f ->
             "ServiceUpdateType", ServiceUpdateType.to_json f)
       ; Aws.Util.option_map v.service_update_recommended_apply_by_date (fun f ->
             "ServiceUpdateRecommendedApplyByDate", DateTime.to_json f)
       ; Aws.Util.option_map v.service_update_status (fun f ->
             "ServiceUpdateStatus", ServiceUpdateStatus.to_json f)
       ; Aws.Util.option_map v.service_update_severity (fun f ->
             "ServiceUpdateSeverity", ServiceUpdateSeverity.to_json f)
       ; Aws.Util.option_map v.service_update_release_date (fun f ->
             "ServiceUpdateReleaseDate", DateTime.to_json f)
       ; Aws.Util.option_map v.service_update_name (fun f ->
             "ServiceUpdateName", String.to_json f)
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             "CacheClusterId", String.to_json f)
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             "ReplicationGroupId", String.to_json f)
       ])

let of_json j =
  { replication_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
  ; cache_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
  ; service_update_name =
      Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateName") String.of_json
  ; service_update_release_date =
      Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateReleaseDate") DateTime.of_json
  ; service_update_severity =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServiceUpdateSeverity")
        ServiceUpdateSeverity.of_json
  ; service_update_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServiceUpdateStatus")
        ServiceUpdateStatus.of_json
  ; service_update_recommended_apply_by_date =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServiceUpdateRecommendedApplyByDate")
        DateTime.of_json
  ; service_update_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServiceUpdateType")
        ServiceUpdateType.of_json
  ; update_action_available_date =
      Aws.Util.option_map (Aws.Json.lookup j "UpdateActionAvailableDate") DateTime.of_json
  ; update_action_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "UpdateActionStatus")
        UpdateActionStatus.of_json
  ; nodes_updated = Aws.Util.option_map (Aws.Json.lookup j "NodesUpdated") String.of_json
  ; update_action_status_modified_date =
      Aws.Util.option_map
        (Aws.Json.lookup j "UpdateActionStatusModifiedDate")
        DateTime.of_json
  ; sla_met = Aws.Util.option_map (Aws.Json.lookup j "SlaMet") SlaMet.of_json
  ; node_group_update_status =
      NodeGroupUpdateStatusList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupUpdateStatus"))
  ; cache_node_update_status =
      CacheNodeUpdateStatusList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodeUpdateStatus"))
  ; estimated_update_time =
      Aws.Util.option_map (Aws.Json.lookup j "EstimatedUpdateTime") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  }
