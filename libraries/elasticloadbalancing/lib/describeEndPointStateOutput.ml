type t = { instance_states : InstanceStates.t }

let make ?(instance_states = []) () = { instance_states }

let parse xml =
  Some
    { instance_states =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceStates" xml)
             InstanceStates.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("InstanceStates.member", InstanceStates.to_query v.instance_states))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("InstanceStates", InstanceStates.to_json v.instance_states) ])

let of_json j =
  { instance_states =
      InstanceStates.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceStates"))
  }
