open Aws.BaseTypes

type t =
  { global_replication_group_id : String.t
  ; replication_group_id : String.t
  ; replication_group_region : String.t
  }

let make ~global_replication_group_id ~replication_group_id ~replication_group_region () =
  { global_replication_group_id; replication_group_id; replication_group_region }

let parse xml =
  Some
    { global_replication_group_id =
        Aws.Xml.required
          "GlobalReplicationGroupId"
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalReplicationGroupId" xml)
             String.parse)
    ; replication_group_id =
        Aws.Xml.required
          "ReplicationGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
    ; replication_group_region =
        Aws.Xml.required
          "ReplicationGroupRegion"
          (Aws.Util.option_bind
             (Aws.Xml.member "ReplicationGroupRegion" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ReplicationGroupRegion", String.to_query v.replication_group_region))
       ; Some
           (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
       ; Some
           (Aws.Query.Pair
              ("GlobalReplicationGroupId", String.to_query v.global_replication_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ReplicationGroupRegion", String.to_json v.replication_group_region)
       ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
       ; Some ("GlobalReplicationGroupId", String.to_json v.global_replication_group_id)
       ])

let of_json j =
  { global_replication_group_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupId"))
  ; replication_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
  ; replication_group_region =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupRegion"))
  }
