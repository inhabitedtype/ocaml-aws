open Aws.BaseTypes

type t =
  { global_replication_group_id_suffix : String.t
  ; global_replication_group_description : String.t option
  ; primary_replication_group_id : String.t
  }

let make
    ~global_replication_group_id_suffix
    ?global_replication_group_description
    ~primary_replication_group_id
    () =
  { global_replication_group_id_suffix
  ; global_replication_group_description
  ; primary_replication_group_id
  }

let parse xml =
  Some
    { global_replication_group_id_suffix =
        Aws.Xml.required
          "GlobalReplicationGroupIdSuffix"
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalReplicationGroupIdSuffix" xml)
             String.parse)
    ; global_replication_group_description =
        Aws.Util.option_bind
          (Aws.Xml.member "GlobalReplicationGroupDescription" xml)
          String.parse
    ; primary_replication_group_id =
        Aws.Xml.required
          "PrimaryReplicationGroupId"
          (Aws.Util.option_bind
             (Aws.Xml.member "PrimaryReplicationGroupId" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("PrimaryReplicationGroupId", String.to_query v.primary_replication_group_id))
       ; Aws.Util.option_map v.global_replication_group_description (fun f ->
             Aws.Query.Pair ("GlobalReplicationGroupDescription", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "GlobalReplicationGroupIdSuffix"
              , String.to_query v.global_replication_group_id_suffix ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PrimaryReplicationGroupId", String.to_json v.primary_replication_group_id)
       ; Aws.Util.option_map v.global_replication_group_description (fun f ->
             "GlobalReplicationGroupDescription", String.to_json f)
       ; Some
           ( "GlobalReplicationGroupIdSuffix"
           , String.to_json v.global_replication_group_id_suffix )
       ])

let of_json j =
  { global_replication_group_id_suffix =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupIdSuffix"))
  ; global_replication_group_description =
      Aws.Util.option_map
        (Aws.Json.lookup j "GlobalReplicationGroupDescription")
        String.of_json
  ; primary_replication_group_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PrimaryReplicationGroupId"))
  }
