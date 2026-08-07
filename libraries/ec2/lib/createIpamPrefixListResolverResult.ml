type t = { ipam_prefix_list_resolver : IpamPrefixListResolver.t option }

let make ?ipam_prefix_list_resolver () = { ipam_prefix_list_resolver }

let parse xml =
  Some
    { ipam_prefix_list_resolver =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamPrefixListResolver" xml)
          IpamPrefixListResolver.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_prefix_list_resolver (fun f ->
             Aws.Query.Pair ("IpamPrefixListResolver", IpamPrefixListResolver.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_prefix_list_resolver (fun f ->
             "ipamPrefixListResolver", IpamPrefixListResolver.to_json f)
       ])

let of_json j =
  { ipam_prefix_list_resolver =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamPrefixListResolver")
        IpamPrefixListResolver.of_json
  }
