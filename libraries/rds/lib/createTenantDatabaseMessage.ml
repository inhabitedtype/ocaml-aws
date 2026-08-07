open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t
  ; tenant_d_b_name : String.t
  ; master_username : String.t
  ; master_user_password : String.t option
  ; character_set_name : String.t option
  ; nchar_character_set_name : String.t option
  ; manage_master_user_password : Boolean.t option
  ; master_user_secret_kms_key_id : String.t option
  ; tags : TagList.t
  }

let make
    ~d_b_instance_identifier
    ~tenant_d_b_name
    ~master_username
    ?master_user_password
    ?character_set_name
    ?nchar_character_set_name
    ?manage_master_user_password
    ?master_user_secret_kms_key_id
    ?(tags = [])
    () =
  { d_b_instance_identifier
  ; tenant_d_b_name
  ; master_username
  ; master_user_password
  ; character_set_name
  ; nchar_character_set_name
  ; manage_master_user_password
  ; master_user_secret_kms_key_id
  ; tags
  }

let parse xml =
  Some
    { d_b_instance_identifier =
        Aws.Xml.required
          "DBInstanceIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse)
    ; tenant_d_b_name =
        Aws.Xml.required
          "TenantDBName"
          (Aws.Util.option_bind (Aws.Xml.member "TenantDBName" xml) String.parse)
    ; master_username =
        Aws.Xml.required
          "MasterUsername"
          (Aws.Util.option_bind (Aws.Xml.member "MasterUsername" xml) String.parse)
    ; master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "MasterUserPassword" xml) String.parse
    ; character_set_name =
        Aws.Util.option_bind (Aws.Xml.member "CharacterSetName" xml) String.parse
    ; nchar_character_set_name =
        Aws.Util.option_bind (Aws.Xml.member "NcharCharacterSetName" xml) String.parse
    ; manage_master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "ManageMasterUserPassword" xml) Boolean.parse
    ; master_user_secret_kms_key_id =
        Aws.Util.option_bind (Aws.Xml.member "MasterUserSecretKmsKeyId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.master_user_secret_kms_key_id (fun f ->
             Aws.Query.Pair ("MasterUserSecretKmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.manage_master_user_password (fun f ->
             Aws.Query.Pair ("ManageMasterUserPassword", Boolean.to_query f))
       ; Aws.Util.option_map v.nchar_character_set_name (fun f ->
             Aws.Query.Pair ("NcharCharacterSetName", String.to_query f))
       ; Aws.Util.option_map v.character_set_name (fun f ->
             Aws.Query.Pair ("CharacterSetName", String.to_query f))
       ; Aws.Util.option_map v.master_user_password (fun f ->
             Aws.Query.Pair ("MasterUserPassword", String.to_query f))
       ; Some (Aws.Query.Pair ("MasterUsername", String.to_query v.master_username))
       ; Some (Aws.Query.Pair ("TenantDBName", String.to_query v.tenant_d_b_name))
       ; Some
           (Aws.Query.Pair
              ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.master_user_secret_kms_key_id (fun f ->
             "MasterUserSecretKmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.manage_master_user_password (fun f ->
             "ManageMasterUserPassword", Boolean.to_json f)
       ; Aws.Util.option_map v.nchar_character_set_name (fun f ->
             "NcharCharacterSetName", String.to_json f)
       ; Aws.Util.option_map v.character_set_name (fun f ->
             "CharacterSetName", String.to_json f)
       ; Aws.Util.option_map v.master_user_password (fun f ->
             "MasterUserPassword", String.to_json f)
       ; Some ("MasterUsername", String.to_json v.master_username)
       ; Some ("TenantDBName", String.to_json v.tenant_d_b_name)
       ; Some ("DBInstanceIdentifier", String.to_json v.d_b_instance_identifier)
       ])

let of_json j =
  { d_b_instance_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBInstanceIdentifier"))
  ; tenant_d_b_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TenantDBName"))
  ; master_username =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MasterUsername"))
  ; master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserPassword") String.of_json
  ; character_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "CharacterSetName") String.of_json
  ; nchar_character_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "NcharCharacterSetName") String.of_json
  ; manage_master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "ManageMasterUserPassword") Boolean.of_json
  ; master_user_secret_kms_key_id =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserSecretKmsKeyId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
