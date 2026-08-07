open Aws.BaseTypes

type t =
  { availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; bytes_converted : Long.t option
  ; description : String.t option
  ; image : DiskImageDescription.t option
  ; volume : DiskImageVolumeDescription.t option
  }

let make
    ?availability_zone
    ?availability_zone_id
    ?bytes_converted
    ?description
    ?image
    ?volume
    () =
  { availability_zone; availability_zone_id; bytes_converted; description; image; volume }

let parse xml =
  Some
    { availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; bytes_converted =
        Aws.Util.option_bind (Aws.Xml.member "bytesConverted" xml) Long.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; image = Aws.Util.option_bind (Aws.Xml.member "image" xml) DiskImageDescription.parse
    ; volume =
        Aws.Util.option_bind
          (Aws.Xml.member "volume" xml)
          DiskImageVolumeDescription.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume (fun f ->
             Aws.Query.Pair ("Volume", DiskImageVolumeDescription.to_query f))
       ; Aws.Util.option_map v.image (fun f ->
             Aws.Query.Pair ("Image", DiskImageDescription.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.bytes_converted (fun f ->
             Aws.Query.Pair ("BytesConverted", Long.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume (fun f ->
             "volume", DiskImageVolumeDescription.to_json f)
       ; Aws.Util.option_map v.image (fun f -> "image", DiskImageDescription.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.bytes_converted (fun f -> "bytesConverted", Long.to_json f)
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
  ; bytes_converted =
      Aws.Util.option_map (Aws.Json.lookup j "bytesConverted") Long.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; image = Aws.Util.option_map (Aws.Json.lookup j "image") DiskImageDescription.of_json
  ; volume =
      Aws.Util.option_map (Aws.Json.lookup j "volume") DiskImageVolumeDescription.of_json
  }
