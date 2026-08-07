open Aws.BaseTypes

type t =
  { global_replication_group_id : String.t
  ; node_group_count : Integer.t
  ; regional_configurations : RegionalConfigurationList.t
  ; apply_immediately : Boolean.t
  }

let make
    ~global_replication_group_id
    ~node_group_count
    ?(regional_configurations = [])
    ~apply_immediately
    () =
  { global_replication_group_id
  ; node_group_count
  ; regional_configurations
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
    ; regional_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RegionalConfigurations" xml)
             RegionalConfigurationList.parse)
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
              ( "RegionalConfigurations.member"
              , RegionalConfigurationList.to_query v.regional_configurations ))
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
           ( "RegionalConfigurations"
           , RegionalConfigurationList.to_json v.regional_configurations )
       ; Some ("NodeGroupCount", Integer.to_json v.node_group_count)
       ; Some ("GlobalReplicationGroupId", String.to_json v.global_replication_group_id)
       ])

let of_json j =
  { global_replication_group_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupId"))
  ; node_group_count =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupCount"))
  ; regional_configurations =
      RegionalConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RegionalConfigurations"))
  ; apply_immediately =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyImmediately"))
  }
