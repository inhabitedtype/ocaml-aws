open Aws.BaseTypes

type t =
  { global_replication_group_id : String.t option
  ; global_replication_group_member_role : String.t option
  }

let make ?global_replication_group_id ?global_replication_group_member_role () =
  { global_replication_group_id; global_replication_group_member_role }

let parse xml =
  Some
    { global_replication_group_id =
        Aws.Util.option_bind (Aws.Xml.member "GlobalReplicationGroupId" xml) String.parse
    ; global_replication_group_member_role =
        Aws.Util.option_bind
          (Aws.Xml.member "GlobalReplicationGroupMemberRole" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.global_replication_group_member_role (fun f ->
             Aws.Query.Pair ("GlobalReplicationGroupMemberRole", String.to_query f))
       ; Aws.Util.option_map v.global_replication_group_id (fun f ->
             Aws.Query.Pair ("GlobalReplicationGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.global_replication_group_member_role (fun f ->
             "GlobalReplicationGroupMemberRole", String.to_json f)
       ; Aws.Util.option_map v.global_replication_group_id (fun f ->
             "GlobalReplicationGroupId", String.to_json f)
       ])

let of_json j =
  { global_replication_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "GlobalReplicationGroupId") String.of_json
  ; global_replication_group_member_role =
      Aws.Util.option_map
        (Aws.Json.lookup j "GlobalReplicationGroupMemberRole")
        String.of_json
  }
