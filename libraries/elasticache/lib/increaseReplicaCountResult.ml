type t = { replication_group : ReplicationGroup.t option }

let make ?replication_group () = { replication_group }

let parse xml =
  Some
    { replication_group =
        Aws.Util.option_bind
          (Aws.Xml.member "ReplicationGroup" xml)
          ReplicationGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.replication_group (fun f ->
             Aws.Query.Pair ("ReplicationGroup", ReplicationGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.replication_group (fun f ->
             "ReplicationGroup", ReplicationGroup.to_json f)
       ])

let of_json j =
  { replication_group =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroup") ReplicationGroup.of_json
  }
