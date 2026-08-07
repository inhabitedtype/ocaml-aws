open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t
  ; tenant_d_b_name : String.t
  ; master_user_password : String.t option
  ; new_tenant_d_b_name : String.t option
  ; manage_master_user_password : Boolean.t option
  ; rotate_master_user_password : Boolean.t option
  ; master_user_secret_kms_key_id : String.t option
  }

let make
    ~d_b_instance_identifier
    ~tenant_d_b_name
    ?master_user_password
    ?new_tenant_d_b_name
    ?manage_master_user_password
    ?rotate_master_user_password
    ?master_user_secret_kms_key_id
    () =
  { d_b_instance_identifier
  ; tenant_d_b_name
  ; master_user_password
  ; new_tenant_d_b_name
  ; manage_master_user_password
  ; rotate_master_user_password
  ; master_user_secret_kms_key_id
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
    ; master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "MasterUserPassword" xml) String.parse
    ; new_tenant_d_b_name =
        Aws.Util.option_bind (Aws.Xml.member "NewTenantDBName" xml) String.parse
    ; manage_master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "ManageMasterUserPassword" xml) Boolean.parse
    ; rotate_master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "RotateMasterUserPassword" xml) Boolean.parse
    ; master_user_secret_kms_key_id =
        Aws.Util.option_bind (Aws.Xml.member "MasterUserSecretKmsKeyId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.master_user_secret_kms_key_id (fun f ->
             Aws.Query.Pair ("MasterUserSecretKmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.rotate_master_user_password (fun f ->
             Aws.Query.Pair ("RotateMasterUserPassword", Boolean.to_query f))
       ; Aws.Util.option_map v.manage_master_user_password (fun f ->
             Aws.Query.Pair ("ManageMasterUserPassword", Boolean.to_query f))
       ; Aws.Util.option_map v.new_tenant_d_b_name (fun f ->
             Aws.Query.Pair ("NewTenantDBName", String.to_query f))
       ; Aws.Util.option_map v.master_user_password (fun f ->
             Aws.Query.Pair ("MasterUserPassword", String.to_query f))
       ; Some (Aws.Query.Pair ("TenantDBName", String.to_query v.tenant_d_b_name))
       ; Some
           (Aws.Query.Pair
              ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.master_user_secret_kms_key_id (fun f ->
             "MasterUserSecretKmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.rotate_master_user_password (fun f ->
             "RotateMasterUserPassword", Boolean.to_json f)
       ; Aws.Util.option_map v.manage_master_user_password (fun f ->
             "ManageMasterUserPassword", Boolean.to_json f)
       ; Aws.Util.option_map v.new_tenant_d_b_name (fun f ->
             "NewTenantDBName", String.to_json f)
       ; Aws.Util.option_map v.master_user_password (fun f ->
             "MasterUserPassword", String.to_json f)
       ; Some ("TenantDBName", String.to_json v.tenant_d_b_name)
       ; Some ("DBInstanceIdentifier", String.to_json v.d_b_instance_identifier)
       ])

let of_json j =
  { d_b_instance_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBInstanceIdentifier"))
  ; tenant_d_b_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TenantDBName"))
  ; master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserPassword") String.of_json
  ; new_tenant_d_b_name =
      Aws.Util.option_map (Aws.Json.lookup j "NewTenantDBName") String.of_json
  ; manage_master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "ManageMasterUserPassword") Boolean.of_json
  ; rotate_master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "RotateMasterUserPassword") Boolean.of_json
  ; master_user_secret_kms_key_id =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserSecretKmsKeyId") String.of_json
  }
