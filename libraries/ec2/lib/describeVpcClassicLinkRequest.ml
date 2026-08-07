open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; vpc_ids : VpcClassicLinkIdList.t
  ; filters : FilterList.t
  }

let make ?dry_run ?(vpc_ids = []) ?(filters = []) () = { dry_run; vpc_ids; filters }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; vpc_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) VpcClassicLinkIdList.parse)
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Some (Aws.Query.Pair ("VpcId", VpcClassicLinkIdList.to_query v.vpc_ids))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Some ("VpcId", VpcClassicLinkIdList.to_json v.vpc_ids)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; vpc_ids =
      VpcClassicLinkIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcId"))
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
