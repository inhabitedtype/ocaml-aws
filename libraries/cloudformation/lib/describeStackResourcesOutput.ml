type t = { stack_resources : StackResources.t }

let make ?(stack_resources = []) () = { stack_resources }

let parse xml =
  Some
    { stack_resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "StackResources" xml)
             StackResources.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("StackResources.member", StackResources.to_query v.stack_resources))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("StackResources", StackResources.to_json v.stack_resources) ])

let of_json j =
  { stack_resources =
      StackResources.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResources"))
  }
