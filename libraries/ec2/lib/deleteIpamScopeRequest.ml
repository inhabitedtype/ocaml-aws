open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_scope_id : String.t
  }

let make ?dry_run ~ipam_scope_id () = { dry_run; ipam_scope_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_scope_id =
        Aws.Xml.required
          "IpamScopeId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamScopeId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("IpamScopeId", String.to_query v.ipam_scope_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("IpamScopeId", String.to_json v.ipam_scope_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_scope_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamScopeId"))
  }
