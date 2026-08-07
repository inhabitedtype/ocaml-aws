type t = { ipam_resource_discovery : IpamResourceDiscovery.t option }

let make ?ipam_resource_discovery () = { ipam_resource_discovery }

let parse xml =
  Some
    { ipam_resource_discovery =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamResourceDiscovery" xml)
          IpamResourceDiscovery.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_resource_discovery (fun f ->
             Aws.Query.Pair ("IpamResourceDiscovery", IpamResourceDiscovery.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_resource_discovery (fun f ->
             "ipamResourceDiscovery", IpamResourceDiscovery.to_json f)
       ])

let of_json j =
  { ipam_resource_discovery =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamResourceDiscovery")
        IpamResourceDiscovery.of_json
  }
