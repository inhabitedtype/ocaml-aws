open Aws.BaseTypes

type t =
  { global_replication_group_id : String.t
  ; apply_immediately : Boolean.t
  }

let make ~global_replication_group_id ~apply_immediately () =
  { global_replication_group_id; apply_immediately }

let parse xml =
  Some
    { global_replication_group_id =
        Aws.Xml.required
          "GlobalReplicationGroupId"
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalReplicationGroupId" xml)
             String.parse)
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
              ("GlobalReplicationGroupId", String.to_query v.global_replication_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ApplyImmediately", Boolean.to_json v.apply_immediately)
       ; Some ("GlobalReplicationGroupId", String.to_json v.global_replication_group_id)
       ])

let of_json j =
  { global_replication_group_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupId"))
  ; apply_immediately =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyImmediately"))
  }
