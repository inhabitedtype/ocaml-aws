open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; network_acl_id : String.t
  }

let make ?dry_run ~network_acl_id () = { dry_run; network_acl_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; network_acl_id =
        Aws.Xml.required
          "networkAclId"
          (Aws.Util.option_bind (Aws.Xml.member "networkAclId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("NetworkAclId", String.to_query v.network_acl_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("networkAclId", String.to_json v.network_acl_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; network_acl_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "networkAclId"))
  }
