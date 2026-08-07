open Aws.BaseTypes

type t =
  { volume_name : String.t option
  ; storage : ValidStorageOptionsList.t
  }

let make ?volume_name ?(storage = []) () = { volume_name; storage }

let parse xml =
  Some
    { volume_name = Aws.Util.option_bind (Aws.Xml.member "VolumeName" xml) String.parse
    ; storage =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Storage" xml)
             ValidStorageOptionsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Storage.member", ValidStorageOptionsList.to_query v.storage))
       ; Aws.Util.option_map v.volume_name (fun f ->
             Aws.Query.Pair ("VolumeName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Storage", ValidStorageOptionsList.to_json v.storage)
       ; Aws.Util.option_map v.volume_name (fun f -> "VolumeName", String.to_json f)
       ])

let of_json j =
  { volume_name = Aws.Util.option_map (Aws.Json.lookup j "VolumeName") String.of_json
  ; storage =
      ValidStorageOptionsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Storage"))
  }
