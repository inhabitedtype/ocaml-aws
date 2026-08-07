open Aws.BaseTypes

type t =
  { marker : String.t option
  ; tenant_databases : TenantDatabasesList.t
  }

let make ?marker ?(tenant_databases = []) () = { marker; tenant_databases }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; tenant_databases =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TenantDatabases" xml)
             TenantDatabasesList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TenantDatabases.member", TenantDatabasesList.to_query v.tenant_databases))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TenantDatabases", TenantDatabasesList.to_json v.tenant_databases)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; tenant_databases =
      TenantDatabasesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TenantDatabases"))
  }
