open Aws.BaseTypes

type t =
  { replication_group_id : String.t
  ; replication_group_region : String.t
  ; resharding_configuration : ReshardingConfigurationList.t
  }

let make ~replication_group_id ~replication_group_region ~resharding_configuration () =
  { replication_group_id; replication_group_region; resharding_configuration }

let parse xml =
  Some
    { replication_group_id =
        Aws.Xml.required
          "ReplicationGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
    ; replication_group_region =
        Aws.Xml.required
          "ReplicationGroupRegion"
          (Aws.Util.option_bind
             (Aws.Xml.member "ReplicationGroupRegion" xml)
             String.parse)
    ; resharding_configuration =
        Aws.Xml.required
          "ReshardingConfiguration"
          (Aws.Util.option_bind
             (Aws.Xml.member "ReshardingConfiguration" xml)
             ReshardingConfigurationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ReshardingConfiguration.member"
              , ReshardingConfigurationList.to_query v.resharding_configuration ))
       ; Some
           (Aws.Query.Pair
              ("ReplicationGroupRegion", String.to_query v.replication_group_region))
       ; Some
           (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ReshardingConfiguration"
           , ReshardingConfigurationList.to_json v.resharding_configuration )
       ; Some ("ReplicationGroupRegion", String.to_json v.replication_group_region)
       ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
       ])

let of_json j =
  { replication_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
  ; replication_group_region =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupRegion"))
  ; resharding_configuration =
      ReshardingConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReshardingConfiguration"))
  }
