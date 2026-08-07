type t = { volume_modification : VolumeModification.t option }

let make ?volume_modification () = { volume_modification }

let parse xml =
  Some
    { volume_modification =
        Aws.Util.option_bind
          (Aws.Xml.member "volumeModification" xml)
          VolumeModification.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume_modification (fun f ->
             Aws.Query.Pair ("VolumeModification", VolumeModification.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume_modification (fun f ->
             "volumeModification", VolumeModification.to_json f)
       ])

let of_json j =
  { volume_modification =
      Aws.Util.option_map
        (Aws.Json.lookup j "volumeModification")
        VolumeModification.of_json
  }
