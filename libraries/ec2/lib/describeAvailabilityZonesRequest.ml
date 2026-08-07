open Aws.BaseTypes

type t =
  { zone_names : ZoneNameStringList.t
  ; zone_ids : ZoneIdStringList.t
  ; all_availability_zones : Boolean.t option
  ; dry_run : Boolean.t option
  ; filters : FilterList.t
  }

let make
    ?(zone_names = [])
    ?(zone_ids = [])
    ?all_availability_zones
    ?dry_run
    ?(filters = [])
    () =
  { zone_names; zone_ids; all_availability_zones; dry_run; filters }

let parse xml =
  Some
    { zone_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ZoneName" xml) ZoneNameStringList.parse)
    ; zone_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ZoneId" xml) ZoneIdStringList.parse)
    ; all_availability_zones =
        Aws.Util.option_bind (Aws.Xml.member "AllAvailabilityZones" xml) Boolean.parse
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
       ; Aws.Util.option_map v.all_availability_zones (fun f ->
             Aws.Query.Pair ("AllAvailabilityZones", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("ZoneId", ZoneIdStringList.to_query v.zone_ids))
       ; Some (Aws.Query.Pair ("ZoneName", ZoneNameStringList.to_query v.zone_names))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.all_availability_zones (fun f ->
             "AllAvailabilityZones", Boolean.to_json f)
       ; Some ("ZoneId", ZoneIdStringList.to_json v.zone_ids)
       ; Some ("ZoneName", ZoneNameStringList.to_json v.zone_names)
       ])

let of_json j =
  { zone_names =
      ZoneNameStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ZoneName"))
  ; zone_ids =
      ZoneIdStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ZoneId"))
  ; all_availability_zones =
      Aws.Util.option_map (Aws.Json.lookup j "AllAvailabilityZones") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
