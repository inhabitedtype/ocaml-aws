type t = { starting_instances : InstanceStateChangeList.t }

let make ?(starting_instances = []) () = { starting_instances }

let parse xml =
  Some
    { starting_instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instancesSet" xml)
             InstanceStateChangeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("InstancesSet", InstanceStateChangeList.to_query v.starting_instances))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("instancesSet", InstanceStateChangeList.to_json v.starting_instances) ])

let of_json j =
  { starting_instances =
      InstanceStateChangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instancesSet"))
  }
