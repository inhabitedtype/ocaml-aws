open Aws.BaseTypes

type t =
  { availability_zone_id : String.t option
  ; dry_run : Boolean.t option
  ; availability_zone : String.t option
  ; image : DiskImageDetail.t
  ; description : String.t option
  ; volume : VolumeDetail.t
  }

let make ?availability_zone_id ?dry_run ?availability_zone ~image ?description ~volume ()
    =
  { availability_zone_id; dry_run; availability_zone; image; description; volume }

let parse xml =
  Some
    { availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; image =
        Aws.Xml.required
          "image"
          (Aws.Util.option_bind (Aws.Xml.member "image" xml) DiskImageDetail.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; volume =
        Aws.Xml.required
          "volume"
          (Aws.Util.option_bind (Aws.Xml.member "volume" xml) VolumeDetail.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Volume", VolumeDetail.to_query v.volume))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some (Aws.Query.Pair ("Image", DiskImageDetail.to_query v.image))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("volume", VolumeDetail.to_json v.volume)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Some ("image", DiskImageDetail.to_json v.image)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ])

let of_json j =
  { availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; image = DiskImageDetail.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "image"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; volume = VolumeDetail.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "volume"))
  }
