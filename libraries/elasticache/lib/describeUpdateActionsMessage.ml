open Aws.BaseTypes

type t =
  { service_update_name : String.t option
  ; replication_group_ids : ReplicationGroupIdList.t
  ; cache_cluster_ids : CacheClusterIdList.t
  ; engine : String.t option
  ; service_update_status : ServiceUpdateStatusList.t
  ; service_update_time_range : TimeRangeFilter.t option
  ; update_action_status : UpdateActionStatusList.t
  ; show_node_level_update_status : Boolean.t option
  ; max_records : Integer.t option
  ; marker : String.t option
  }

let make
    ?service_update_name
    ?(replication_group_ids = [])
    ?(cache_cluster_ids = [])
    ?engine
    ?(service_update_status = [])
    ?service_update_time_range
    ?(update_action_status = [])
    ?show_node_level_update_status
    ?max_records
    ?marker
    () =
  { service_update_name
  ; replication_group_ids
  ; cache_cluster_ids
  ; engine
  ; service_update_status
  ; service_update_time_range
  ; update_action_status
  ; show_node_level_update_status
  ; max_records
  ; marker
  }

let parse xml =
  Some
    { service_update_name =
        Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateName" xml) String.parse
    ; replication_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReplicationGroupIds" xml)
             ReplicationGroupIdList.parse)
    ; cache_cluster_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheClusterIds" xml)
             CacheClusterIdList.parse)
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; service_update_status =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ServiceUpdateStatus" xml)
             ServiceUpdateStatusList.parse)
    ; service_update_time_range =
        Aws.Util.option_bind
          (Aws.Xml.member "ServiceUpdateTimeRange" xml)
          TimeRangeFilter.parse
    ; update_action_status =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UpdateActionStatus" xml)
             UpdateActionStatusList.parse)
    ; show_node_level_update_status =
        Aws.Util.option_bind
          (Aws.Xml.member "ShowNodeLevelUpdateStatus" xml)
          Boolean.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.show_node_level_update_status (fun f ->
             Aws.Query.Pair ("ShowNodeLevelUpdateStatus", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "UpdateActionStatus.member"
              , UpdateActionStatusList.to_query v.update_action_status ))
       ; Aws.Util.option_map v.service_update_time_range (fun f ->
             Aws.Query.Pair ("ServiceUpdateTimeRange", TimeRangeFilter.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ServiceUpdateStatus.member"
              , ServiceUpdateStatusList.to_query v.service_update_status ))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CacheClusterIds.member", CacheClusterIdList.to_query v.cache_cluster_ids))
       ; Some
           (Aws.Query.Pair
              ( "ReplicationGroupIds.member"
              , ReplicationGroupIdList.to_query v.replication_group_ids ))
       ; Aws.Util.option_map v.service_update_name (fun f ->
             Aws.Query.Pair ("ServiceUpdateName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.show_node_level_update_status (fun f ->
             "ShowNodeLevelUpdateStatus", Boolean.to_json f)
       ; Some ("UpdateActionStatus", UpdateActionStatusList.to_json v.update_action_status)
       ; Aws.Util.option_map v.service_update_time_range (fun f ->
             "ServiceUpdateTimeRange", TimeRangeFilter.to_json f)
       ; Some
           ("ServiceUpdateStatus", ServiceUpdateStatusList.to_json v.service_update_status)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Some ("CacheClusterIds", CacheClusterIdList.to_json v.cache_cluster_ids)
       ; Some
           ("ReplicationGroupIds", ReplicationGroupIdList.to_json v.replication_group_ids)
       ; Aws.Util.option_map v.service_update_name (fun f ->
             "ServiceUpdateName", String.to_json f)
       ])

let of_json j =
  { service_update_name =
      Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateName") String.of_json
  ; replication_group_ids =
      ReplicationGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupIds"))
  ; cache_cluster_ids =
      CacheClusterIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheClusterIds"))
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; service_update_status =
      ServiceUpdateStatusList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ServiceUpdateStatus"))
  ; service_update_time_range =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServiceUpdateTimeRange")
        TimeRangeFilter.of_json
  ; update_action_status =
      UpdateActionStatusList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "UpdateActionStatus"))
  ; show_node_level_update_status =
      Aws.Util.option_map (Aws.Json.lookup j "ShowNodeLevelUpdateStatus") Boolean.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
