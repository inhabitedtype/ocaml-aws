open Aws.BaseTypes

type t = { failed_stack_instances_count : Integer.t option }

let make ?failed_stack_instances_count () = { failed_stack_instances_count }

let parse xml =
  Some
    { failed_stack_instances_count =
        Aws.Util.option_bind
          (Aws.Xml.member "FailedStackInstancesCount" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.failed_stack_instances_count (fun f ->
             Aws.Query.Pair ("FailedStackInstancesCount", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.failed_stack_instances_count (fun f ->
             "FailedStackInstancesCount", Integer.to_json f)
       ])

let of_json j =
  { failed_stack_instances_count =
      Aws.Util.option_map (Aws.Json.lookup j "FailedStackInstancesCount") Integer.of_json
  }
