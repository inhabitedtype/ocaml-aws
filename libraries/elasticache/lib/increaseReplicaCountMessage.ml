open Aws.BaseTypes

type t =
  { replication_group_id : String.t
  ; new_replica_count : Integer.t option
  ; replica_configuration : ReplicaConfigurationList.t
  ; apply_immediately : Boolean.t
  }

let make
    ~replication_group_id
    ?new_replica_count
    ?(replica_configuration = [])
    ~apply_immediately
    () =
  { replication_group_id; new_replica_count; replica_configuration; apply_immediately }

let parse xml =
  Some
    { replication_group_id =
        Aws.Xml.required
          "ReplicationGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
    ; new_replica_count =
        Aws.Util.option_bind (Aws.Xml.member "NewReplicaCount" xml) Integer.parse
    ; replica_configuration =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReplicaConfiguration" xml)
             ReplicaConfigurationList.parse)
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
              ( "ReplicaConfiguration.member"
              , ReplicaConfigurationList.to_query v.replica_configuration ))
       ; Aws.Util.option_map v.new_replica_count (fun f ->
             Aws.Query.Pair ("NewReplicaCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ApplyImmediately", Boolean.to_json v.apply_immediately)
       ; Some
           ( "ReplicaConfiguration"
           , ReplicaConfigurationList.to_json v.replica_configuration )
       ; Aws.Util.option_map v.new_replica_count (fun f ->
             "NewReplicaCount", Integer.to_json f)
       ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
       ])

let of_json j =
  { replication_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
  ; new_replica_count =
      Aws.Util.option_map (Aws.Json.lookup j "NewReplicaCount") Integer.of_json
  ; replica_configuration =
      ReplicaConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicaConfiguration"))
  ; apply_immediately =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyImmediately"))
  }
