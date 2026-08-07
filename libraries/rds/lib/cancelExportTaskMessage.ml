open Aws.BaseTypes

type t = { export_task_identifier : String.t }

let make ~export_task_identifier () = { export_task_identifier }

let parse xml =
  Some
    { export_task_identifier =
        Aws.Xml.required
          "ExportTaskIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "ExportTaskIdentifier" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ExportTaskIdentifier", String.to_query v.export_task_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ExportTaskIdentifier", String.to_json v.export_task_identifier) ])

let of_json j =
  { export_task_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ExportTaskIdentifier"))
  }
