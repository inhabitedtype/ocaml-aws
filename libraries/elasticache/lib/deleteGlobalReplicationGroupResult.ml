type t = { global_replication_group : GlobalReplicationGroup.t option }

let make ?global_replication_group () = { global_replication_group }

let parse xml =
  Some
    { global_replication_group =
        Aws.Util.option_bind
          (Aws.Xml.member "GlobalReplicationGroup" xml)
          GlobalReplicationGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.global_replication_group (fun f ->
             Aws.Query.Pair ("GlobalReplicationGroup", GlobalReplicationGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.global_replication_group (fun f ->
             "GlobalReplicationGroup", GlobalReplicationGroup.to_json f)
       ])

let of_json j =
  { global_replication_group =
      Aws.Util.option_map
        (Aws.Json.lookup j "GlobalReplicationGroup")
        GlobalReplicationGroup.of_json
  }
