open Aws.BaseTypes

type t =
  { replication_group_id : String.t
  ; node_group_id : String.t
  }

let make ~replication_group_id ~node_group_id () = { replication_group_id; node_group_id }

let parse xml =
  Some
    { replication_group_id =
        Aws.Xml.required
          "ReplicationGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
    ; node_group_id =
        Aws.Xml.required
          "NodeGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("NodeGroupId", String.to_query v.node_group_id))
       ; Some
           (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("NodeGroupId", String.to_json v.node_group_id)
       ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
       ])

let of_json j =
  { replication_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
  ; node_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupId"))
  }
