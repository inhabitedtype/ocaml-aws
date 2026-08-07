type t = { host_ids : ResponseHostIdList.t }

let make ?(host_ids = []) () = { host_ids }

let parse xml =
  Some
    { host_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "hostIdSet" xml) ResponseHostIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("HostIdSet", ResponseHostIdList.to_query v.host_ids)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("hostIdSet", ResponseHostIdList.to_json v.host_ids) ])

let of_json j =
  { host_ids =
      ResponseHostIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "hostIdSet"))
  }
