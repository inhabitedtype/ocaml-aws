type t = { ipam_policy : IpamPolicy.t option }

let make ?ipam_policy () = { ipam_policy }

let parse xml =
  Some
    { ipam_policy =
        Aws.Util.option_bind (Aws.Xml.member "ipamPolicy" xml) IpamPolicy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_policy (fun f ->
             Aws.Query.Pair ("IpamPolicy", IpamPolicy.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_policy (fun f -> "ipamPolicy", IpamPolicy.to_json f) ])

let of_json j =
  { ipam_policy = Aws.Util.option_map (Aws.Json.lookup j "ipamPolicy") IpamPolicy.of_json
  }
