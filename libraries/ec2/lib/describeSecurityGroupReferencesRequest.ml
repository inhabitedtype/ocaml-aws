open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; group_id : GroupIds.t
  }

let make ?dry_run ~group_id () = { dry_run; group_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; group_id =
        Aws.Xml.required
          "GroupId"
          (Aws.Util.option_bind (Aws.Xml.member "GroupId" xml) GroupIds.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("GroupId", GroupIds.to_query v.group_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("GroupId", GroupIds.to_json v.group_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; group_id = GroupIds.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GroupId"))
  }
