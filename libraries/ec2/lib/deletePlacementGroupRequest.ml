open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; group_name : String.t
  }

let make ?dry_run ~group_name () = { dry_run; group_name }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; group_name =
        Aws.Xml.required
          "groupName"
          (Aws.Util.option_bind (Aws.Xml.member "groupName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("GroupName", String.to_query v.group_name))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("groupName", String.to_json v.group_name)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; group_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupName"))
  }
