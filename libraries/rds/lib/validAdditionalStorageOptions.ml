open Aws.BaseTypes

type t =
  { supports_additional_storage_volumes : Boolean.t option
  ; volumes : ValidVolumeOptionsList.t
  }

let make ?supports_additional_storage_volumes ?(volumes = []) () =
  { supports_additional_storage_volumes; volumes }

let parse xml =
  Some
    { supports_additional_storage_volumes =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsAdditionalStorageVolumes" xml)
          Boolean.parse
    ; volumes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Volumes" xml)
             ValidVolumeOptionsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Volumes.member", ValidVolumeOptionsList.to_query v.volumes))
       ; Aws.Util.option_map v.supports_additional_storage_volumes (fun f ->
             Aws.Query.Pair ("SupportsAdditionalStorageVolumes", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Volumes", ValidVolumeOptionsList.to_json v.volumes)
       ; Aws.Util.option_map v.supports_additional_storage_volumes (fun f ->
             "SupportsAdditionalStorageVolumes", Boolean.to_json f)
       ])

let of_json j =
  { supports_additional_storage_volumes =
      Aws.Util.option_map
        (Aws.Json.lookup j "SupportsAdditionalStorageVolumes")
        Boolean.of_json
  ; volumes =
      ValidVolumeOptionsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Volumes"))
  }
