open Aws.BaseTypes

type t =
  { global_replication_group_id : String.t
  ; retain_primary_replication_group : Boolean.t
  }

let make ~global_replication_group_id ~retain_primary_replication_group () =
  { global_replication_group_id; retain_primary_replication_group }

let parse xml =
  Some
    { global_replication_group_id =
        Aws.Xml.required
          "GlobalReplicationGroupId"
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalReplicationGroupId" xml)
             String.parse)
    ; retain_primary_replication_group =
        Aws.Xml.required
          "RetainPrimaryReplicationGroup"
          (Aws.Util.option_bind
             (Aws.Xml.member "RetainPrimaryReplicationGroup" xml)
             Boolean.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "RetainPrimaryReplicationGroup"
              , Boolean.to_query v.retain_primary_replication_group ))
       ; Some
           (Aws.Query.Pair
              ("GlobalReplicationGroupId", String.to_query v.global_replication_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "RetainPrimaryReplicationGroup"
           , Boolean.to_json v.retain_primary_replication_group )
       ; Some ("GlobalReplicationGroupId", String.to_json v.global_replication_group_id)
       ])

let of_json j =
  { global_replication_group_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupId"))
  ; retain_primary_replication_group =
      Boolean.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RetainPrimaryReplicationGroup"))
  }
