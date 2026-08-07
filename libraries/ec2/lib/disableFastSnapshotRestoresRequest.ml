open Aws.BaseTypes

type t =
  { availability_zones : AvailabilityZoneStringList.t
  ; availability_zone_ids : AvailabilityZoneIdStringList.t
  ; source_snapshot_ids : SnapshotIdStringList.t
  ; dry_run : Boolean.t option
  }

let make
    ?(availability_zones = [])
    ?(availability_zone_ids = [])
    ~source_snapshot_ids
    ?dry_run
    () =
  { availability_zones; availability_zone_ids; source_snapshot_ids; dry_run }

let parse xml =
  Some
    { availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailabilityZone" xml)
             AvailabilityZoneStringList.parse)
    ; availability_zone_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailabilityZoneId" xml)
             AvailabilityZoneIdStringList.parse)
    ; source_snapshot_ids =
        Aws.Xml.required
          "SourceSnapshotId"
          (Aws.Util.option_bind
             (Aws.Xml.member "SourceSnapshotId" xml)
             SnapshotIdStringList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SourceSnapshotId", SnapshotIdStringList.to_query v.source_snapshot_ids))
       ; Some
           (Aws.Query.Pair
              ( "AvailabilityZoneId"
              , AvailabilityZoneIdStringList.to_query v.availability_zone_ids ))
       ; Some
           (Aws.Query.Pair
              ( "AvailabilityZone"
              , AvailabilityZoneStringList.to_query v.availability_zones ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("SourceSnapshotId", SnapshotIdStringList.to_json v.source_snapshot_ids)
       ; Some
           ( "AvailabilityZoneId"
           , AvailabilityZoneIdStringList.to_json v.availability_zone_ids )
       ; Some ("AvailabilityZone", AvailabilityZoneStringList.to_json v.availability_zones)
       ])

let of_json j =
  { availability_zones =
      AvailabilityZoneStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZone"))
  ; availability_zone_ids =
      AvailabilityZoneIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZoneId"))
  ; source_snapshot_ids =
      SnapshotIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceSnapshotId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
