open Aws.BaseTypes

type t =
  { bundle_ids : BundleIdStringList.t
  ; dry_run : Boolean.t option
  ; filters : FilterList.t
  }

let make ?(bundle_ids = []) ?dry_run ?(filters = []) () = { bundle_ids; dry_run; filters }

let parse xml =
  Some
    { bundle_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "BundleId" xml) BundleIdStringList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("BundleId", BundleIdStringList.to_query v.bundle_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("BundleId", BundleIdStringList.to_json v.bundle_ids)
       ])

let of_json j =
  { bundle_ids =
      BundleIdStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "BundleId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
