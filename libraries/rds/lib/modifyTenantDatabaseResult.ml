type t = { tenant_database : TenantDatabase.t option }

let make ?tenant_database () = { tenant_database }

let parse xml =
  Some
    { tenant_database =
        Aws.Util.option_bind (Aws.Xml.member "TenantDatabase" xml) TenantDatabase.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.tenant_database (fun f ->
             Aws.Query.Pair ("TenantDatabase", TenantDatabase.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.tenant_database (fun f ->
             "TenantDatabase", TenantDatabase.to_json f)
       ])

let of_json j =
  { tenant_database =
      Aws.Util.option_map (Aws.Json.lookup j "TenantDatabase") TenantDatabase.of_json
  }
