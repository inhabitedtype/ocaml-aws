open Aws.BaseTypes

type t =
  { d_b_snapshot_identifier : String.t option
  ; d_b_instance_identifier : String.t option
  ; dbi_resource_id : String.t option
  ; engine_name : String.t option
  ; snapshot_type : String.t option
  ; tenant_database_create_time : DateTime.t option
  ; tenant_d_b_name : String.t option
  ; master_username : String.t option
  ; tenant_database_resource_id : String.t option
  ; character_set_name : String.t option
  ; d_b_snapshot_tenant_database_a_r_n : String.t option
  ; nchar_character_set_name : String.t option
  ; tag_list : TagList.t
  }

let make
    ?d_b_snapshot_identifier
    ?d_b_instance_identifier
    ?dbi_resource_id
    ?engine_name
    ?snapshot_type
    ?tenant_database_create_time
    ?tenant_d_b_name
    ?master_username
    ?tenant_database_resource_id
    ?character_set_name
    ?d_b_snapshot_tenant_database_a_r_n
    ?nchar_character_set_name
    ?(tag_list = [])
    () =
  { d_b_snapshot_identifier
  ; d_b_instance_identifier
  ; dbi_resource_id
  ; engine_name
  ; snapshot_type
  ; tenant_database_create_time
  ; tenant_d_b_name
  ; master_username
  ; tenant_database_resource_id
  ; character_set_name
  ; d_b_snapshot_tenant_database_a_r_n
  ; nchar_character_set_name
  ; tag_list
  }

let parse xml =
  Some
    { d_b_snapshot_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBSnapshotIdentifier" xml) String.parse
    ; d_b_instance_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse
    ; dbi_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "DbiResourceId" xml) String.parse
    ; engine_name = Aws.Util.option_bind (Aws.Xml.member "EngineName" xml) String.parse
    ; snapshot_type =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotType" xml) String.parse
    ; tenant_database_create_time =
        Aws.Util.option_bind
          (Aws.Xml.member "TenantDatabaseCreateTime" xml)
          DateTime.parse
    ; tenant_d_b_name =
        Aws.Util.option_bind (Aws.Xml.member "TenantDBName" xml) String.parse
    ; master_username =
        Aws.Util.option_bind (Aws.Xml.member "MasterUsername" xml) String.parse
    ; tenant_database_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "TenantDatabaseResourceId" xml) String.parse
    ; character_set_name =
        Aws.Util.option_bind (Aws.Xml.member "CharacterSetName" xml) String.parse
    ; d_b_snapshot_tenant_database_a_r_n =
        Aws.Util.option_bind
          (Aws.Xml.member "DBSnapshotTenantDatabaseARN" xml)
          String.parse
    ; nchar_character_set_name =
        Aws.Util.option_bind (Aws.Xml.member "NcharCharacterSetName" xml) String.parse
    ; tag_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagList" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagList.member", TagList.to_query v.tag_list))
       ; Aws.Util.option_map v.nchar_character_set_name (fun f ->
             Aws.Query.Pair ("NcharCharacterSetName", String.to_query f))
       ; Aws.Util.option_map v.d_b_snapshot_tenant_database_a_r_n (fun f ->
             Aws.Query.Pair ("DBSnapshotTenantDatabaseARN", String.to_query f))
       ; Aws.Util.option_map v.character_set_name (fun f ->
             Aws.Query.Pair ("CharacterSetName", String.to_query f))
       ; Aws.Util.option_map v.tenant_database_resource_id (fun f ->
             Aws.Query.Pair ("TenantDatabaseResourceId", String.to_query f))
       ; Aws.Util.option_map v.master_username (fun f ->
             Aws.Query.Pair ("MasterUsername", String.to_query f))
       ; Aws.Util.option_map v.tenant_d_b_name (fun f ->
             Aws.Query.Pair ("TenantDBName", String.to_query f))
       ; Aws.Util.option_map v.tenant_database_create_time (fun f ->
             Aws.Query.Pair ("TenantDatabaseCreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.snapshot_type (fun f ->
             Aws.Query.Pair ("SnapshotType", String.to_query f))
       ; Aws.Util.option_map v.engine_name (fun f ->
             Aws.Query.Pair ("EngineName", String.to_query f))
       ; Aws.Util.option_map v.dbi_resource_id (fun f ->
             Aws.Query.Pair ("DbiResourceId", String.to_query f))
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             Aws.Query.Pair ("DBInstanceIdentifier", String.to_query f))
       ; Aws.Util.option_map v.d_b_snapshot_identifier (fun f ->
             Aws.Query.Pair ("DBSnapshotIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagList", TagList.to_json v.tag_list)
       ; Aws.Util.option_map v.nchar_character_set_name (fun f ->
             "NcharCharacterSetName", String.to_json f)
       ; Aws.Util.option_map v.d_b_snapshot_tenant_database_a_r_n (fun f ->
             "DBSnapshotTenantDatabaseARN", String.to_json f)
       ; Aws.Util.option_map v.character_set_name (fun f ->
             "CharacterSetName", String.to_json f)
       ; Aws.Util.option_map v.tenant_database_resource_id (fun f ->
             "TenantDatabaseResourceId", String.to_json f)
       ; Aws.Util.option_map v.master_username (fun f ->
             "MasterUsername", String.to_json f)
       ; Aws.Util.option_map v.tenant_d_b_name (fun f -> "TenantDBName", String.to_json f)
       ; Aws.Util.option_map v.tenant_database_create_time (fun f ->
             "TenantDatabaseCreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.snapshot_type (fun f -> "SnapshotType", String.to_json f)
       ; Aws.Util.option_map v.engine_name (fun f -> "EngineName", String.to_json f)
       ; Aws.Util.option_map v.dbi_resource_id (fun f ->
             "DbiResourceId", String.to_json f)
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             "DBInstanceIdentifier", String.to_json f)
       ; Aws.Util.option_map v.d_b_snapshot_identifier (fun f ->
             "DBSnapshotIdentifier", String.to_json f)
       ])

let of_json j =
  { d_b_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBSnapshotIdentifier") String.of_json
  ; d_b_instance_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceIdentifier") String.of_json
  ; dbi_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "DbiResourceId") String.of_json
  ; engine_name = Aws.Util.option_map (Aws.Json.lookup j "EngineName") String.of_json
  ; snapshot_type = Aws.Util.option_map (Aws.Json.lookup j "SnapshotType") String.of_json
  ; tenant_database_create_time =
      Aws.Util.option_map (Aws.Json.lookup j "TenantDatabaseCreateTime") DateTime.of_json
  ; tenant_d_b_name =
      Aws.Util.option_map (Aws.Json.lookup j "TenantDBName") String.of_json
  ; master_username =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUsername") String.of_json
  ; tenant_database_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "TenantDatabaseResourceId") String.of_json
  ; character_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "CharacterSetName") String.of_json
  ; d_b_snapshot_tenant_database_a_r_n =
      Aws.Util.option_map (Aws.Json.lookup j "DBSnapshotTenantDatabaseARN") String.of_json
  ; nchar_character_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "NcharCharacterSetName") String.of_json
  ; tag_list = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagList"))
  }
