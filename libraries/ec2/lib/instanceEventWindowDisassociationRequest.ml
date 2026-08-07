type t =
  { instance_ids : InstanceIdList.t
  ; instance_tags : TagList.t
  ; dedicated_host_ids : DedicatedHostIdList.t
  }

let make ?(instance_ids = []) ?(instance_tags = []) ?(dedicated_host_ids = []) () =
  { instance_ids; instance_tags; dedicated_host_ids }

let parse xml =
  Some
    { instance_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) InstanceIdList.parse)
    ; instance_tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "InstanceTag" xml) TagList.parse)
    ; dedicated_host_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DedicatedHostId" xml)
             DedicatedHostIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("DedicatedHostId", DedicatedHostIdList.to_query v.dedicated_host_ids))
       ; Some (Aws.Query.Pair ("InstanceTag", TagList.to_query v.instance_tags))
       ; Some (Aws.Query.Pair ("InstanceId", InstanceIdList.to_query v.instance_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DedicatedHostId", DedicatedHostIdList.to_json v.dedicated_host_ids)
       ; Some ("InstanceTag", TagList.to_json v.instance_tags)
       ; Some ("InstanceId", InstanceIdList.to_json v.instance_ids)
       ])

let of_json j =
  { instance_ids =
      InstanceIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; instance_tags =
      TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceTag"))
  ; dedicated_host_ids =
      DedicatedHostIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DedicatedHostId"))
  }
