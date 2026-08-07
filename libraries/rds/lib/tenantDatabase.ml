open Aws.BaseTypes

type t =
  { tenant_database_create_time : DateTime.t option
  ; d_b_instance_identifier : String.t option
  ; tenant_d_b_name : String.t option
  ; status : String.t option
  ; master_username : String.t option
  ; dbi_resource_id : String.t option
  ; tenant_database_resource_id : String.t option
  ; tenant_database_a_r_n : String.t option
  ; character_set_name : String.t option
  ; nchar_character_set_name : String.t option
  ; deletion_protection : Boolean.t option
  ; pending_modified_values : TenantDatabasePendingModifiedValues.t option
  ; master_user_secret : MasterUserSecret.t option
  ; tag_list : TagList.t
  }

let make
    ?tenant_database_create_time
    ?d_b_instance_identifier
    ?tenant_d_b_name
    ?status
    ?master_username
    ?dbi_resource_id
    ?tenant_database_resource_id
    ?tenant_database_a_r_n
    ?character_set_name
    ?nchar_character_set_name
    ?deletion_protection
    ?pending_modified_values
    ?master_user_secret
    ?(tag_list = [])
    () =
  { tenant_database_create_time
  ; d_b_instance_identifier
  ; tenant_d_b_name
  ; status
  ; master_username
  ; dbi_resource_id
  ; tenant_database_resource_id
  ; tenant_database_a_r_n
  ; character_set_name
  ; nchar_character_set_name
  ; deletion_protection
  ; pending_modified_values
  ; master_user_secret
  ; tag_list
  }

let parse xml =
  Some
    { tenant_database_create_time =
        Aws.Util.option_bind
          (Aws.Xml.member "TenantDatabaseCreateTime" xml)
          DateTime.parse
    ; d_b_instance_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse
    ; tenant_d_b_name =
        Aws.Util.option_bind (Aws.Xml.member "TenantDBName" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; master_username =
        Aws.Util.option_bind (Aws.Xml.member "MasterUsername" xml) String.parse
    ; dbi_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "DbiResourceId" xml) String.parse
    ; tenant_database_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "TenantDatabaseResourceId" xml) String.parse
    ; tenant_database_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "TenantDatabaseARN" xml) String.parse
    ; character_set_name =
        Aws.Util.option_bind (Aws.Xml.member "CharacterSetName" xml) String.parse
    ; nchar_character_set_name =
        Aws.Util.option_bind (Aws.Xml.member "NcharCharacterSetName" xml) String.parse
    ; deletion_protection =
        Aws.Util.option_bind (Aws.Xml.member "DeletionProtection" xml) Boolean.parse
    ; pending_modified_values =
        Aws.Util.option_bind
          (Aws.Xml.member "PendingModifiedValues" xml)
          TenantDatabasePendingModifiedValues.parse
    ; master_user_secret =
        Aws.Util.option_bind
          (Aws.Xml.member "MasterUserSecret" xml)
          MasterUserSecret.parse
    ; tag_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagList" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagList.member", TagList.to_query v.tag_list))
       ; Aws.Util.option_map v.master_user_secret (fun f ->
             Aws.Query.Pair ("MasterUserSecret", MasterUserSecret.to_query f))
       ; Aws.Util.option_map v.pending_modified_values (fun f ->
             Aws.Query.Pair
               ("PendingModifiedValues", TenantDatabasePendingModifiedValues.to_query f))
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             Aws.Query.Pair ("DeletionProtection", Boolean.to_query f))
       ; Aws.Util.option_map v.nchar_character_set_name (fun f ->
             Aws.Query.Pair ("NcharCharacterSetName", String.to_query f))
       ; Aws.Util.option_map v.character_set_name (fun f ->
             Aws.Query.Pair ("CharacterSetName", String.to_query f))
       ; Aws.Util.option_map v.tenant_database_a_r_n (fun f ->
             Aws.Query.Pair ("TenantDatabaseARN", String.to_query f))
       ; Aws.Util.option_map v.tenant_database_resource_id (fun f ->
             Aws.Query.Pair ("TenantDatabaseResourceId", String.to_query f))
       ; Aws.Util.option_map v.dbi_resource_id (fun f ->
             Aws.Query.Pair ("DbiResourceId", String.to_query f))
       ; Aws.Util.option_map v.master_username (fun f ->
             Aws.Query.Pair ("MasterUsername", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.tenant_d_b_name (fun f ->
             Aws.Query.Pair ("TenantDBName", String.to_query f))
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             Aws.Query.Pair ("DBInstanceIdentifier", String.to_query f))
       ; Aws.Util.option_map v.tenant_database_create_time (fun f ->
             Aws.Query.Pair ("TenantDatabaseCreateTime", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagList", TagList.to_json v.tag_list)
       ; Aws.Util.option_map v.master_user_secret (fun f ->
             "MasterUserSecret", MasterUserSecret.to_json f)
       ; Aws.Util.option_map v.pending_modified_values (fun f ->
             "PendingModifiedValues", TenantDatabasePendingModifiedValues.to_json f)
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             "DeletionProtection", Boolean.to_json f)
       ; Aws.Util.option_map v.nchar_character_set_name (fun f ->
             "NcharCharacterSetName", String.to_json f)
       ; Aws.Util.option_map v.character_set_name (fun f ->
             "CharacterSetName", String.to_json f)
       ; Aws.Util.option_map v.tenant_database_a_r_n (fun f ->
             "TenantDatabaseARN", String.to_json f)
       ; Aws.Util.option_map v.tenant_database_resource_id (fun f ->
             "TenantDatabaseResourceId", String.to_json f)
       ; Aws.Util.option_map v.dbi_resource_id (fun f ->
             "DbiResourceId", String.to_json f)
       ; Aws.Util.option_map v.master_username (fun f ->
             "MasterUsername", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.tenant_d_b_name (fun f -> "TenantDBName", String.to_json f)
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             "DBInstanceIdentifier", String.to_json f)
       ; Aws.Util.option_map v.tenant_database_create_time (fun f ->
             "TenantDatabaseCreateTime", DateTime.to_json f)
       ])

let of_json j =
  { tenant_database_create_time =
      Aws.Util.option_map (Aws.Json.lookup j "TenantDatabaseCreateTime") DateTime.of_json
  ; d_b_instance_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceIdentifier") String.of_json
  ; tenant_d_b_name =
      Aws.Util.option_map (Aws.Json.lookup j "TenantDBName") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; master_username =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUsername") String.of_json
  ; dbi_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "DbiResourceId") String.of_json
  ; tenant_database_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "TenantDatabaseResourceId") String.of_json
  ; tenant_database_a_r_n =
      Aws.Util.option_map (Aws.Json.lookup j "TenantDatabaseARN") String.of_json
  ; character_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "CharacterSetName") String.of_json
  ; nchar_character_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "NcharCharacterSetName") String.of_json
  ; deletion_protection =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionProtection") Boolean.of_json
  ; pending_modified_values =
      Aws.Util.option_map
        (Aws.Json.lookup j "PendingModifiedValues")
        TenantDatabasePendingModifiedValues.of_json
  ; master_user_secret =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserSecret") MasterUserSecret.of_json
  ; tag_list = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagList"))
  }
