open Aws.BaseTypes

type t =
  { master_user_password : String.t option
  ; tenant_d_b_name : String.t option
  }

let make ?master_user_password ?tenant_d_b_name () =
  { master_user_password; tenant_d_b_name }

let parse xml =
  Some
    { master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "MasterUserPassword" xml) String.parse
    ; tenant_d_b_name =
        Aws.Util.option_bind (Aws.Xml.member "TenantDBName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.tenant_d_b_name (fun f ->
             Aws.Query.Pair ("TenantDBName", String.to_query f))
       ; Aws.Util.option_map v.master_user_password (fun f ->
             Aws.Query.Pair ("MasterUserPassword", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.tenant_d_b_name (fun f -> "TenantDBName", String.to_json f)
       ; Aws.Util.option_map v.master_user_password (fun f ->
             "MasterUserPassword", String.to_json f)
       ])

let of_json j =
  { master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserPassword") String.of_json
  ; tenant_d_b_name =
      Aws.Util.option_map (Aws.Json.lookup j "TenantDBName") String.of_json
  }
