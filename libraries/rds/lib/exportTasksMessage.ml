open Aws.BaseTypes

type t =
  { marker : String.t option
  ; export_tasks : ExportTasksList.t
  }

let make ?marker ?(export_tasks = []) () = { marker; export_tasks }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; export_tasks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ExportTasks" xml) ExportTasksList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("ExportTasks.member", ExportTasksList.to_query v.export_tasks))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ExportTasks", ExportTasksList.to_json v.export_tasks)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; export_tasks =
      ExportTasksList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ExportTasks"))
  }
