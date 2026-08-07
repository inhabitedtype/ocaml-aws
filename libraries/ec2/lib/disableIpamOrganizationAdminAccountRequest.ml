open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; delegated_admin_account_id : String.t
  }

let make ?dry_run ~delegated_admin_account_id () = { dry_run; delegated_admin_account_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; delegated_admin_account_id =
        Aws.Xml.required
          "DelegatedAdminAccountId"
          (Aws.Util.option_bind
             (Aws.Xml.member "DelegatedAdminAccountId" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("DelegatedAdminAccountId", String.to_query v.delegated_admin_account_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DelegatedAdminAccountId", String.to_json v.delegated_admin_account_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; delegated_admin_account_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DelegatedAdminAccountId"))
  }
