type t = { stack_resource_detail : StackResourceDetail.t option }

let make ?stack_resource_detail () = { stack_resource_detail }

let parse xml =
  Some
    { stack_resource_detail =
        Aws.Util.option_bind
          (Aws.Xml.member "StackResourceDetail" xml)
          StackResourceDetail.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_resource_detail (fun f ->
             Aws.Query.Pair ("StackResourceDetail", StackResourceDetail.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_resource_detail (fun f ->
             "StackResourceDetail", StackResourceDetail.to_json f)
       ])

let of_json j =
  { stack_resource_detail =
      Aws.Util.option_map
        (Aws.Json.lookup j "StackResourceDetail")
        StackResourceDetail.of_json
  }
