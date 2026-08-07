type t =
  { instance_ids : InstanceIdList.t
  ; tags : TagList.t
  ; dedicated_host_ids : DedicatedHostIdList.t
  }

let make ?(instance_ids = []) ?(tags = []) ?(dedicated_host_ids = []) () =
  { instance_ids; tags; dedicated_host_ids }

let parse xml =
  Some
    { instance_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "instanceIdSet" xml) InstanceIdList.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; dedicated_host_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "dedicatedHostIdSet" xml)
             DedicatedHostIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("DedicatedHostIdSet", DedicatedHostIdList.to_query v.dedicated_host_ids))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("InstanceIdSet", InstanceIdList.to_query v.instance_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("dedicatedHostIdSet", DedicatedHostIdList.to_json v.dedicated_host_ids)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Some ("instanceIdSet", InstanceIdList.to_json v.instance_ids)
       ])

let of_json j =
  { instance_ids =
      InstanceIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceIdSet"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; dedicated_host_ids =
      DedicatedHostIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "dedicatedHostIdSet"))
  }
