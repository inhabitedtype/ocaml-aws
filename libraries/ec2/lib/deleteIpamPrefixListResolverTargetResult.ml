type t = { ipam_prefix_list_resolver_target : IpamPrefixListResolverTarget.t option }

let make ?ipam_prefix_list_resolver_target () = { ipam_prefix_list_resolver_target }

let parse xml =
  Some
    { ipam_prefix_list_resolver_target =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamPrefixListResolverTarget" xml)
          IpamPrefixListResolverTarget.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_prefix_list_resolver_target (fun f ->
             Aws.Query.Pair
               ("IpamPrefixListResolverTarget", IpamPrefixListResolverTarget.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_prefix_list_resolver_target (fun f ->
             "ipamPrefixListResolverTarget", IpamPrefixListResolverTarget.to_json f)
       ])

let of_json j =
  { ipam_prefix_list_resolver_target =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamPrefixListResolverTarget")
        IpamPrefixListResolverTarget.of_json
  }
