type t = { stack_resource_drift : StackResourceDrift.t }

let make ~stack_resource_drift () = { stack_resource_drift }

let parse xml =
  Some
    { stack_resource_drift =
        Aws.Xml.required
          "StackResourceDrift"
          (Aws.Util.option_bind
             (Aws.Xml.member "StackResourceDrift" xml)
             StackResourceDrift.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("StackResourceDrift", StackResourceDrift.to_query v.stack_resource_drift))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("StackResourceDrift", StackResourceDrift.to_json v.stack_resource_drift) ])

let of_json j =
  { stack_resource_drift =
      StackResourceDrift.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResourceDrift"))
  }
