open Aws.BaseTypes

type t = { task_handle : String.t option }

let make ?task_handle () = { task_handle }

let parse xml =
  Some
    { task_handle = Aws.Util.option_bind (Aws.Xml.member "TaskHandle" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.task_handle (fun f ->
             Aws.Query.Pair ("TaskHandle", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.task_handle (fun f -> "TaskHandle", String.to_json f) ])

let of_json j =
  { task_handle = Aws.Util.option_map (Aws.Json.lookup j "TaskHandle") String.of_json }
