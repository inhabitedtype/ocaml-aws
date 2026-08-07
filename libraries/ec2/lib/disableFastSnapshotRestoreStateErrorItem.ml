open Aws.BaseTypes

type t =
  { availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; error : DisableFastSnapshotRestoreStateError.t option
  }

let make ?availability_zone ?availability_zone_id ?error () =
  { availability_zone; availability_zone_id; error }

let parse xml =
  Some
    { availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; error =
        Aws.Util.option_bind
          (Aws.Xml.member "error" xml)
          DisableFastSnapshotRestoreStateError.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error (fun f ->
             Aws.Query.Pair ("Error", DisableFastSnapshotRestoreStateError.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error (fun f ->
             "error", DisableFastSnapshotRestoreStateError.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ])

let of_json j =
  { availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; error =
      Aws.Util.option_map
        (Aws.Json.lookup j "error")
        DisableFastSnapshotRestoreStateError.of_json
  }
