open Aws.BaseTypes

type t =
  { global_replication_group_id : String.t option
  ; global_replication_group_description : String.t option
  ; status : String.t option
  ; cache_node_type : String.t option
  ; engine : String.t option
  ; engine_version : String.t option
  ; members : GlobalReplicationGroupMemberList.t
  ; cluster_enabled : Boolean.t option
  ; global_node_groups : GlobalNodeGroupList.t
  ; auth_token_enabled : Boolean.t option
  ; transit_encryption_enabled : Boolean.t option
  ; at_rest_encryption_enabled : Boolean.t option
  ; a_r_n : String.t option
  }

let make
    ?global_replication_group_id
    ?global_replication_group_description
    ?status
    ?cache_node_type
    ?engine
    ?engine_version
    ?(members = [])
    ?cluster_enabled
    ?(global_node_groups = [])
    ?auth_token_enabled
    ?transit_encryption_enabled
    ?at_rest_encryption_enabled
    ?a_r_n
    () =
  { global_replication_group_id
  ; global_replication_group_description
  ; status
  ; cache_node_type
  ; engine
  ; engine_version
  ; members
  ; cluster_enabled
  ; global_node_groups
  ; auth_token_enabled
  ; transit_encryption_enabled
  ; at_rest_encryption_enabled
  ; a_r_n
  }

let parse xml =
  Some
    { global_replication_group_id =
        Aws.Util.option_bind (Aws.Xml.member "GlobalReplicationGroupId" xml) String.parse
    ; global_replication_group_description =
        Aws.Util.option_bind
          (Aws.Xml.member "GlobalReplicationGroupDescription" xml)
          String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; cache_node_type =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; members =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Members" xml)
             GlobalReplicationGroupMemberList.parse)
    ; cluster_enabled =
        Aws.Util.option_bind (Aws.Xml.member "ClusterEnabled" xml) Boolean.parse
    ; global_node_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalNodeGroups" xml)
             GlobalNodeGroupList.parse)
    ; auth_token_enabled =
        Aws.Util.option_bind (Aws.Xml.member "AuthTokenEnabled" xml) Boolean.parse
    ; transit_encryption_enabled =
        Aws.Util.option_bind (Aws.Xml.member "TransitEncryptionEnabled" xml) Boolean.parse
    ; at_rest_encryption_enabled =
        Aws.Util.option_bind (Aws.Xml.member "AtRestEncryptionEnabled" xml) Boolean.parse
    ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.a_r_n (fun f -> Aws.Query.Pair ("ARN", String.to_query f))
       ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
             Aws.Query.Pair ("AtRestEncryptionEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             Aws.Query.Pair ("TransitEncryptionEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.auth_token_enabled (fun f ->
             Aws.Query.Pair ("AuthTokenEnabled", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "GlobalNodeGroups.member"
              , GlobalNodeGroupList.to_query v.global_node_groups ))
       ; Aws.Util.option_map v.cluster_enabled (fun f ->
             Aws.Query.Pair ("ClusterEnabled", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Members.member", GlobalReplicationGroupMemberList.to_query v.members))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             Aws.Query.Pair ("CacheNodeType", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.global_replication_group_description (fun f ->
             Aws.Query.Pair ("GlobalReplicationGroupDescription", String.to_query f))
       ; Aws.Util.option_map v.global_replication_group_id (fun f ->
             Aws.Query.Pair ("GlobalReplicationGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
       ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
             "AtRestEncryptionEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             "TransitEncryptionEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.auth_token_enabled (fun f ->
             "AuthTokenEnabled", Boolean.to_json f)
       ; Some ("GlobalNodeGroups", GlobalNodeGroupList.to_json v.global_node_groups)
       ; Aws.Util.option_map v.cluster_enabled (fun f ->
             "ClusterEnabled", Boolean.to_json f)
       ; Some ("Members", GlobalReplicationGroupMemberList.to_json v.members)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             "CacheNodeType", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.global_replication_group_description (fun f ->
             "GlobalReplicationGroupDescription", String.to_json f)
       ; Aws.Util.option_map v.global_replication_group_id (fun f ->
             "GlobalReplicationGroupId", String.to_json f)
       ])

let of_json j =
  { global_replication_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "GlobalReplicationGroupId") String.of_json
  ; global_replication_group_description =
      Aws.Util.option_map
        (Aws.Json.lookup j "GlobalReplicationGroupDescription")
        String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; cache_node_type =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; members =
      GlobalReplicationGroupMemberList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Members"))
  ; cluster_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "ClusterEnabled") Boolean.of_json
  ; global_node_groups =
      GlobalNodeGroupList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalNodeGroups"))
  ; auth_token_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "AuthTokenEnabled") Boolean.of_json
  ; transit_encryption_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "TransitEncryptionEnabled") Boolean.of_json
  ; at_rest_encryption_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "AtRestEncryptionEnabled") Boolean.of_json
  ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
  }
