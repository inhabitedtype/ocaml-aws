type t =
  { filters : FilterList.t
  ; export_task_ids : ExportTaskIdStringList.t
  }

let make ?(filters = []) ?(export_task_ids = []) () = { filters; export_task_ids }

let parse xml =
  Some
    { filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; export_task_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "exportTaskId" xml)
             ExportTaskIdStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ExportTaskId", ExportTaskIdStringList.to_query v.export_task_ids))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("exportTaskId", ExportTaskIdStringList.to_json v.export_task_ids)
       ; Some ("Filter", FilterList.to_json v.filters)
       ])

let of_json j =
  { filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; export_task_ids =
      ExportTaskIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "exportTaskId"))
  }
