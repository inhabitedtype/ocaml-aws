open Aws.BaseTypes

type t = { dry_run : Boolean.t option }

let make ?dry_run () = { dry_run }

let parse xml =
  Some { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f) ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json }
