type t = { stopping_instances : InstanceStateChangeList.t }

let make ?(stopping_instances = []) () = { stopping_instances }

let parse xml =
  Some
    { stopping_instances =
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
              ("InstancesSet", InstanceStateChangeList.to_query v.stopping_instances))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("instancesSet", InstanceStateChangeList.to_json v.stopping_instances) ])

let of_json j =
  { stopping_instances =
      InstanceStateChangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instancesSet"))
  }
