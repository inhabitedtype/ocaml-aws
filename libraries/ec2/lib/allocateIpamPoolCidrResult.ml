type t = { ipam_pool_allocation : IpamPoolAllocation.t option }

let make ?ipam_pool_allocation () = { ipam_pool_allocation }

let parse xml =
  Some
    { ipam_pool_allocation =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamPoolAllocation" xml)
          IpamPoolAllocation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_pool_allocation (fun f ->
             Aws.Query.Pair ("IpamPoolAllocation", IpamPoolAllocation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_pool_allocation (fun f ->
             "ipamPoolAllocation", IpamPoolAllocation.to_json f)
       ])

let of_json j =
  { ipam_pool_allocation =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamPoolAllocation")
        IpamPoolAllocation.of_json
  }
