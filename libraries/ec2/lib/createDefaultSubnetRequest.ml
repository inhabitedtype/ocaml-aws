open Aws.BaseTypes

type t =
  { availability_zone : String.t option
  ; dry_run : Boolean.t option
  ; ipv6_native : Boolean.t option
  ; availability_zone_id : String.t option
  }

let make ?availability_zone ?dry_run ?ipv6_native ?availability_zone_id () =
  { availability_zone; dry_run; ipv6_native; availability_zone_id }

let parse xml =
  Some
    { availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipv6_native = Aws.Util.option_bind (Aws.Xml.member "Ipv6Native" xml) Boolean.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.ipv6_native (fun f ->
             Aws.Query.Pair ("Ipv6Native", Boolean.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.ipv6_native (fun f -> "Ipv6Native", Boolean.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ])

let of_json j =
  { availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipv6_native = Aws.Util.option_map (Aws.Json.lookup j "Ipv6Native") Boolean.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  }
