open Aws.BaseTypes

type t =
  { key_id : String.t
  ; grant_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~key_id ~grant_id ?dry_run () = { key_id; grant_id; dry_run }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; grant_id =
        Aws.Xml.required
          "GrantId"
          (Aws.Util.option_bind (Aws.Xml.member "GrantId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("GrantId", String.to_query v.grant_id))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("GrantId", String.to_json v.grant_id)
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; grant_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
