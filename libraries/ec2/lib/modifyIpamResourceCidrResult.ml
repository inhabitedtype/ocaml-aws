type t = { ipam_resource_cidr : IpamResourceCidr.t option }

let make ?ipam_resource_cidr () = { ipam_resource_cidr }

let parse xml =
  Some
    { ipam_resource_cidr =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamResourceCidr" xml)
          IpamResourceCidr.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_resource_cidr (fun f ->
             Aws.Query.Pair ("IpamResourceCidr", IpamResourceCidr.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_resource_cidr (fun f ->
             "ipamResourceCidr", IpamResourceCidr.to_json f)
       ])

let of_json j =
  { ipam_resource_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "ipamResourceCidr") IpamResourceCidr.of_json
  }
