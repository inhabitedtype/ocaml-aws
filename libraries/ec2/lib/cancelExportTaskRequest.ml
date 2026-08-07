open Aws.BaseTypes

type t = { export_task_id : String.t }

let make ~export_task_id () = { export_task_id }

let parse xml =
  Some
    { export_task_id =
        Aws.Xml.required
          "exportTaskId"
          (Aws.Util.option_bind (Aws.Xml.member "exportTaskId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ExportTaskId", String.to_query v.export_task_id)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("exportTaskId", String.to_json v.export_task_id) ])

let of_json j =
  { export_task_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "exportTaskId"))
  }
