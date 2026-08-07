type t =
  { ipam_resource_discovery_association : IpamResourceDiscoveryAssociation.t option }

let make ?ipam_resource_discovery_association () = { ipam_resource_discovery_association }

let parse xml =
  Some
    { ipam_resource_discovery_association =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamResourceDiscoveryAssociation" xml)
          IpamResourceDiscoveryAssociation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_resource_discovery_association (fun f ->
             Aws.Query.Pair
               ( "IpamResourceDiscoveryAssociation"
               , IpamResourceDiscoveryAssociation.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_resource_discovery_association (fun f ->
             ( "ipamResourceDiscoveryAssociation"
             , IpamResourceDiscoveryAssociation.to_json f ))
       ])

let of_json j =
  { ipam_resource_discovery_association =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamResourceDiscoveryAssociation")
        IpamResourceDiscoveryAssociation.of_json
  }
