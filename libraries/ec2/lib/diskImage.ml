open Aws.BaseTypes

type t =
  { description : String.t option
  ; image : DiskImageDetail.t option
  ; volume : VolumeDetail.t option
  }

let make ?description ?image ?volume () = { description; image; volume }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; image = Aws.Util.option_bind (Aws.Xml.member "Image" xml) DiskImageDetail.parse
    ; volume = Aws.Util.option_bind (Aws.Xml.member "Volume" xml) VolumeDetail.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume (fun f ->
             Aws.Query.Pair ("Volume", VolumeDetail.to_query f))
       ; Aws.Util.option_map v.image (fun f ->
             Aws.Query.Pair ("Image", DiskImageDetail.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume (fun f -> "Volume", VolumeDetail.to_json f)
       ; Aws.Util.option_map v.image (fun f -> "Image", DiskImageDetail.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; image = Aws.Util.option_map (Aws.Json.lookup j "Image") DiskImageDetail.of_json
  ; volume = Aws.Util.option_map (Aws.Json.lookup j "Volume") VolumeDetail.of_json
  }
