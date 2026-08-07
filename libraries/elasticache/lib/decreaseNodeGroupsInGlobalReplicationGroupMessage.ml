open Aws.BaseTypes

type t =
  { global_replication_group_id : String.t
  ; node_group_count : Integer.t
  ; global_node_groups_to_remove : GlobalNodeGroupIdList.t
  ; global_node_groups_to_retain : GlobalNodeGroupIdList.t
  ; apply_immediately : Boolean.t
  }

let make
    ~global_replication_group_id
    ~node_group_count
    ?(global_node_groups_to_remove = [])
    ?(global_node_groups_to_retain = [])
    ~apply_immediately
    () =
  { global_replication_group_id
  ; node_group_count
  ; global_node_groups_to_remove
  ; global_node_groups_to_retain
  ; apply_immediately
  }

let parse xml =
  Some
    { global_replication_group_id =
        Aws.Xml.required
          "GlobalReplicationGroupId"
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalReplicationGroupId" xml)
             String.parse)
    ; node_group_count =
        Aws.Xml.required
          "NodeGroupCount"
          (Aws.Util.option_bind (Aws.Xml.member "NodeGroupCount" xml) Integer.parse)
    ; global_node_groups_to_remove =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalNodeGroupsToRemove" xml)
             GlobalNodeGroupIdList.parse)
    ; global_node_groups_to_retain =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalNodeGroupsToRetain" xml)
             GlobalNodeGroupIdList.parse)
    ; apply_immediately =
        Aws.Xml.required
          "ApplyImmediately"
          (Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ApplyImmediately", Boolean.to_query v.apply_immediately))
       ; Some
           (Aws.Query.Pair
              ( "GlobalNodeGroupsToRetain.member"
              , GlobalNodeGroupIdList.to_query v.global_node_groups_to_retain ))
       ; Some
           (Aws.Query.Pair
              ( "GlobalNodeGroupsToRemove.member"
              , GlobalNodeGroupIdList.to_query v.global_node_groups_to_remove ))
       ; Some (Aws.Query.Pair ("NodeGroupCount", Integer.to_query v.node_group_count))
       ; Some
           (Aws.Query.Pair
              ("GlobalReplicationGroupId", String.to_query v.global_replication_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ApplyImmediately", Boolean.to_json v.apply_immediately)
       ; Some
           ( "GlobalNodeGroupsToRetain"
           , GlobalNodeGroupIdList.to_json v.global_node_groups_to_retain )
       ; Some
           ( "GlobalNodeGroupsToRemove"
           , GlobalNodeGroupIdList.to_json v.global_node_groups_to_remove )
       ; Some ("NodeGroupCount", Integer.to_json v.node_group_count)
       ; Some ("GlobalReplicationGroupId", String.to_json v.global_replication_group_id)
       ])

let of_json j =
  { global_replication_group_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupId"))
  ; node_group_count =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupCount"))
  ; global_node_groups_to_remove =
      GlobalNodeGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalNodeGroupsToRemove"))
  ; global_node_groups_to_retain =
      GlobalNodeGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalNodeGroupsToRetain"))
  ; apply_immediately =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyImmediately"))
  }
