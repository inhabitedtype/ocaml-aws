type t = { ipam_pool_cidr : IpamPoolCidr.t option }

let make ?ipam_pool_cidr () = { ipam_pool_cidr }

let parse xml =
  Some
    { ipam_pool_cidr =
        Aws.Util.option_bind (Aws.Xml.member "ipamPoolCidr" xml) IpamPoolCidr.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_pool_cidr (fun f ->
             Aws.Query.Pair ("IpamPoolCidr", IpamPoolCidr.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_pool_cidr (fun f ->
             "ipamPoolCidr", IpamPoolCidr.to_json f)
       ])

let of_json j =
  { ipam_pool_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "ipamPoolCidr") IpamPoolCidr.of_json
  }
