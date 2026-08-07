type t = { mac_modification_task : MacModificationTask.t option }

let make ?mac_modification_task () = { mac_modification_task }

let parse xml =
  Some
    { mac_modification_task =
        Aws.Util.option_bind
          (Aws.Xml.member "macModificationTask" xml)
          MacModificationTask.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.mac_modification_task (fun f ->
             Aws.Query.Pair ("MacModificationTask", MacModificationTask.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.mac_modification_task (fun f ->
             "macModificationTask", MacModificationTask.to_json f)
       ])

let of_json j =
  { mac_modification_task =
      Aws.Util.option_map
        (Aws.Json.lookup j "macModificationTask")
        MacModificationTask.of_json
  }
