open Aws.BaseTypes

type t =
  { region_names : RegionNameStringList.t
  ; all_regions : Boolean.t option
  ; dry_run : Boolean.t option
  ; filters : FilterList.t
  }

let make ?(region_names = []) ?all_regions ?dry_run ?(filters = []) () =
  { region_names; all_regions; dry_run; filters }

let parse xml =
  Some
    { region_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RegionName" xml)
             RegionNameStringList.parse)
    ; all_regions = Aws.Util.option_bind (Aws.Xml.member "AllRegions" xml) Boolean.parse
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
       ; Aws.Util.option_map v.all_regions (fun f ->
             Aws.Query.Pair ("AllRegions", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("RegionName", RegionNameStringList.to_query v.region_names))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.all_regions (fun f -> "AllRegions", Boolean.to_json f)
       ; Some ("RegionName", RegionNameStringList.to_json v.region_names)
       ])

let of_json j =
  { region_names =
      RegionNameStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RegionName"))
  ; all_regions = Aws.Util.option_map (Aws.Json.lookup j "AllRegions") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
