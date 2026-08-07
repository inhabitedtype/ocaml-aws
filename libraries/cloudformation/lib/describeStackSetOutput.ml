type t = { stack_set : StackSet.t option }

let make ?stack_set () = { stack_set }

let parse xml =
  Some { stack_set = Aws.Util.option_bind (Aws.Xml.member "StackSet" xml) StackSet.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_set (fun f ->
             Aws.Query.Pair ("StackSet", StackSet.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_set (fun f -> "StackSet", StackSet.to_json f) ])

let of_json j =
  { stack_set = Aws.Util.option_map (Aws.Json.lookup j "StackSet") StackSet.of_json }
