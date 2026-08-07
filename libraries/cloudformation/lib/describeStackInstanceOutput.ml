type t = { stack_instance : StackInstance.t option }

let make ?stack_instance () = { stack_instance }

let parse xml =
  Some
    { stack_instance =
        Aws.Util.option_bind (Aws.Xml.member "StackInstance" xml) StackInstance.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_instance (fun f ->
             Aws.Query.Pair ("StackInstance", StackInstance.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_instance (fun f ->
             "StackInstance", StackInstance.to_json f)
       ])

let of_json j =
  { stack_instance =
      Aws.Util.option_map (Aws.Json.lookup j "StackInstance") StackInstance.of_json
  }
