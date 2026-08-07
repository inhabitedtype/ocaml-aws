type t = { stack_set_operation : StackSetOperation.t option }

let make ?stack_set_operation () = { stack_set_operation }

let parse xml =
  Some
    { stack_set_operation =
        Aws.Util.option_bind
          (Aws.Xml.member "StackSetOperation" xml)
          StackSetOperation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_set_operation (fun f ->
             Aws.Query.Pair ("StackSetOperation", StackSetOperation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_set_operation (fun f ->
             "StackSetOperation", StackSetOperation.to_json f)
       ])

let of_json j =
  { stack_set_operation =
      Aws.Util.option_map
        (Aws.Json.lookup j "StackSetOperation")
        StackSetOperation.of_json
  }
