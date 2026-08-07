open Aws.BaseTypes

type t =
  { bundle_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~bundle_id ?dry_run () = { bundle_id; dry_run }

let parse xml =
  Some
    { bundle_id =
        Aws.Xml.required
          "BundleId"
          (Aws.Util.option_bind (Aws.Xml.member "BundleId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("BundleId", String.to_query v.bundle_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("BundleId", String.to_json v.bundle_id)
       ])

let of_json j =
  { bundle_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "BundleId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
