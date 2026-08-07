open Aws.BaseTypes

type t =
  { group_id : String.t option
  ; group_name : String.t option
  ; dry_run : Boolean.t option
  }

let make ?group_id ?group_name ?dry_run () = { group_id; group_name; dry_run }

let parse xml =
  Some
    { group_id = Aws.Util.option_bind (Aws.Xml.member "GroupId" xml) String.parse
    ; group_name = Aws.Util.option_bind (Aws.Xml.member "GroupName" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ; Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.group_name (fun f -> "GroupName", String.to_json f)
       ; Aws.Util.option_map v.group_id (fun f -> "GroupId", String.to_json f)
       ])

let of_json j =
  { group_id = Aws.Util.option_map (Aws.Json.lookup j "GroupId") String.of_json
  ; group_name = Aws.Util.option_map (Aws.Json.lookup j "GroupName") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
