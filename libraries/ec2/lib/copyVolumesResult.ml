type t = { volumes : VolumeList.t }

let make ?(volumes = []) () = { volumes }

let parse xml =
  Some
    { volumes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "volumeSet" xml) VolumeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("VolumeSet", VolumeList.to_query v.volumes)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("volumeSet", VolumeList.to_json v.volumes) ])

let of_json j =
  { volumes = VolumeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "volumeSet"))
  }
