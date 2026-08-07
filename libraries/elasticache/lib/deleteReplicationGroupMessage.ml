open Aws.BaseTypes

type t =
  { replication_group_id : String.t
  ; retain_primary_cluster : Boolean.t option
  ; final_snapshot_identifier : String.t option
  }

let make ~replication_group_id ?retain_primary_cluster ?final_snapshot_identifier () =
  { replication_group_id; retain_primary_cluster; final_snapshot_identifier }

let parse xml =
  Some
    { replication_group_id =
        Aws.Xml.required
          "ReplicationGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
    ; retain_primary_cluster =
        Aws.Util.option_bind (Aws.Xml.member "RetainPrimaryCluster" xml) Boolean.parse
    ; final_snapshot_identifier =
        Aws.Util.option_bind (Aws.Xml.member "FinalSnapshotIdentifier" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.final_snapshot_identifier (fun f ->
             Aws.Query.Pair ("FinalSnapshotIdentifier", String.to_query f))
       ; Aws.Util.option_map v.retain_primary_cluster (fun f ->
             Aws.Query.Pair ("RetainPrimaryCluster", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.final_snapshot_identifier (fun f ->
             "FinalSnapshotIdentifier", String.to_json f)
       ; Aws.Util.option_map v.retain_primary_cluster (fun f ->
             "RetainPrimaryCluster", Boolean.to_json f)
       ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
       ])

let of_json j =
  { replication_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
  ; retain_primary_cluster =
      Aws.Util.option_map (Aws.Json.lookup j "RetainPrimaryCluster") Boolean.of_json
  ; final_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "FinalSnapshotIdentifier") String.of_json
  }
