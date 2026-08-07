type t = { replace_root_volume_task : ReplaceRootVolumeTask.t option }

let make ?replace_root_volume_task () = { replace_root_volume_task }

let parse xml =
  Some
    { replace_root_volume_task =
        Aws.Util.option_bind
          (Aws.Xml.member "replaceRootVolumeTask" xml)
          ReplaceRootVolumeTask.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.replace_root_volume_task (fun f ->
             Aws.Query.Pair ("ReplaceRootVolumeTask", ReplaceRootVolumeTask.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.replace_root_volume_task (fun f ->
             "replaceRootVolumeTask", ReplaceRootVolumeTask.to_json f)
       ])

let of_json j =
  { replace_root_volume_task =
      Aws.Util.option_map
        (Aws.Json.lookup j "replaceRootVolumeTask")
        ReplaceRootVolumeTask.of_json
  }
