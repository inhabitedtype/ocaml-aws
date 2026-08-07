open Aws.BaseTypes

type t = { task_handle : String.t }

let make ~task_handle () = { task_handle }

let parse xml =
  Some
    { task_handle =
        Aws.Xml.required
          "TaskHandle"
          (Aws.Util.option_bind (Aws.Xml.member "TaskHandle" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TaskHandle", String.to_query v.task_handle)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("TaskHandle", String.to_json v.task_handle) ])

let of_json j =
  { task_handle = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TaskHandle"))
  }
