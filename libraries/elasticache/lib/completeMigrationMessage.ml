open Aws.BaseTypes

type t =
  { replication_group_id : String.t
  ; force : Boolean.t option
  }

let make ~replication_group_id ?force () = { replication_group_id; force }

let parse xml =
  Some
    { replication_group_id =
        Aws.Xml.required
          "ReplicationGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
    ; force = Aws.Util.option_bind (Aws.Xml.member "Force" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.force (fun f ->
             Aws.Query.Pair ("Force", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.force (fun f -> "Force", Boolean.to_json f)
       ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
       ])

let of_json j =
  { replication_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
  ; force = Aws.Util.option_map (Aws.Json.lookup j "Force") Boolean.of_json
  }
