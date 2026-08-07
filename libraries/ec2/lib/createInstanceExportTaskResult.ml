type t = { export_task : ExportTask.t option }

let make ?export_task () = { export_task }

let parse xml =
  Some
    { export_task =
        Aws.Util.option_bind (Aws.Xml.member "exportTask" xml) ExportTask.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.export_task (fun f ->
             Aws.Query.Pair ("ExportTask", ExportTask.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.export_task (fun f -> "exportTask", ExportTask.to_json f) ])

let of_json j =
  { export_task = Aws.Util.option_map (Aws.Json.lookup j "exportTask") ExportTask.of_json
  }
