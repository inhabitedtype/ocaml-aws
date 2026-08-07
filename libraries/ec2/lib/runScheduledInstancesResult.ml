type t = { instance_id_set : InstanceIdSet.t }

let make ?(instance_id_set = []) () = { instance_id_set }

let parse xml =
  Some
    { instance_id_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "instanceIdSet" xml) InstanceIdSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("InstanceIdSet", InstanceIdSet.to_query v.instance_id_set))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("instanceIdSet", InstanceIdSet.to_json v.instance_id_set) ])

let of_json j =
  { instance_id_set =
      InstanceIdSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceIdSet"))
  }
