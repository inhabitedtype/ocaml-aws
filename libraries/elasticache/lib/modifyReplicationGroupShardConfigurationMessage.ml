open Aws.BaseTypes

type t =
  { replication_group_id : String.t
  ; node_group_count : Integer.t
  ; apply_immediately : Boolean.t
  ; resharding_configuration : ReshardingConfigurationList.t
  ; node_groups_to_remove : NodeGroupsToRemoveList.t
  ; node_groups_to_retain : NodeGroupsToRetainList.t
  }

let make
    ~replication_group_id
    ~node_group_count
    ~apply_immediately
    ?(resharding_configuration = [])
    ?(node_groups_to_remove = [])
    ?(node_groups_to_retain = [])
    () =
  { replication_group_id
  ; node_group_count
  ; apply_immediately
  ; resharding_configuration
  ; node_groups_to_remove
  ; node_groups_to_retain
  }

let parse xml =
  Some
    { replication_group_id =
        Aws.Xml.required
          "ReplicationGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
    ; node_group_count =
        Aws.Xml.required
          "NodeGroupCount"
          (Aws.Util.option_bind (Aws.Xml.member "NodeGroupCount" xml) Integer.parse)
    ; apply_immediately =
        Aws.Xml.required
          "ApplyImmediately"
          (Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse)
    ; resharding_configuration =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReshardingConfiguration" xml)
             ReshardingConfigurationList.parse)
    ; node_groups_to_remove =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NodeGroupsToRemove" xml)
             NodeGroupsToRemoveList.parse)
    ; node_groups_to_retain =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NodeGroupsToRetain" xml)
             NodeGroupsToRetainList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "NodeGroupsToRetain.member"
              , NodeGroupsToRetainList.to_query v.node_groups_to_retain ))
       ; Some
           (Aws.Query.Pair
              ( "NodeGroupsToRemove.member"
              , NodeGroupsToRemoveList.to_query v.node_groups_to_remove ))
       ; Some
           (Aws.Query.Pair
              ( "ReshardingConfiguration.member"
              , ReshardingConfigurationList.to_query v.resharding_configuration ))
       ; Some (Aws.Query.Pair ("ApplyImmediately", Boolean.to_query v.apply_immediately))
       ; Some (Aws.Query.Pair ("NodeGroupCount", Integer.to_query v.node_group_count))
       ; Some
           (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("NodeGroupsToRetain", NodeGroupsToRetainList.to_json v.node_groups_to_retain)
       ; Some
           ("NodeGroupsToRemove", NodeGroupsToRemoveList.to_json v.node_groups_to_remove)
       ; Some
           ( "ReshardingConfiguration"
           , ReshardingConfigurationList.to_json v.resharding_configuration )
       ; Some ("ApplyImmediately", Boolean.to_json v.apply_immediately)
       ; Some ("NodeGroupCount", Integer.to_json v.node_group_count)
       ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
       ])

let of_json j =
  { replication_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
  ; node_group_count =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupCount"))
  ; apply_immediately =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyImmediately"))
  ; resharding_configuration =
      ReshardingConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReshardingConfiguration"))
  ; node_groups_to_remove =
      NodeGroupsToRemoveList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupsToRemove"))
  ; node_groups_to_retain =
      NodeGroupsToRetainList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupsToRetain"))
  }
