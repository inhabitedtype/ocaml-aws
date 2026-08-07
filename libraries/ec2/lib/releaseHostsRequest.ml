type t = { host_ids : RequestHostIdList.t }

let make ~host_ids () = { host_ids }

let parse xml =
  Some
    { host_ids =
        Aws.Xml.required
          "hostId"
          (Aws.Util.option_bind (Aws.Xml.member "hostId" xml) RequestHostIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("HostId", RequestHostIdList.to_query v.host_ids)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("hostId", RequestHostIdList.to_json v.host_ids) ])

let of_json j =
  { host_ids =
      RequestHostIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "hostId"))
  }
