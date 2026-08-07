type t = { export_tasks : ExportTaskList.t }

let make ?(export_tasks = []) () = { export_tasks }

let parse xml =
  Some
    { export_tasks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "exportTaskSet" xml) ExportTaskList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ExportTaskSet", ExportTaskList.to_query v.export_tasks)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("exportTaskSet", ExportTaskList.to_json v.export_tasks) ])

let of_json j =
  { export_tasks =
      ExportTaskList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "exportTaskSet"))
  }
