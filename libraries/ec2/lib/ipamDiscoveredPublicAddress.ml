open Aws.BaseTypes

type t =
  { ipam_resource_discovery_id : String.t option
  ; address_region : String.t option
  ; address : String.t option
  ; address_owner_id : String.t option
  ; address_allocation_id : String.t option
  ; association_status : IpamPublicAddressAssociationStatus.t option
  ; address_type : IpamPublicAddressType.t option
  ; service : IpamPublicAddressAwsService.t option
  ; service_resource : String.t option
  ; vpc_id : String.t option
  ; subnet_id : String.t option
  ; public_ipv4_pool_id : String.t option
  ; network_interface_id : String.t option
  ; network_interface_description : String.t option
  ; instance_id : String.t option
  ; tags : IpamPublicAddressTags.t option
  ; network_border_group : String.t option
  ; security_groups : IpamPublicAddressSecurityGroupList.t
  ; sample_time : DateTime.t option
  }

let make
    ?ipam_resource_discovery_id
    ?address_region
    ?address
    ?address_owner_id
    ?address_allocation_id
    ?association_status
    ?address_type
    ?service
    ?service_resource
    ?vpc_id
    ?subnet_id
    ?public_ipv4_pool_id
    ?network_interface_id
    ?network_interface_description
    ?instance_id
    ?tags
    ?network_border_group
    ?(security_groups = [])
    ?sample_time
    () =
  { ipam_resource_discovery_id
  ; address_region
  ; address
  ; address_owner_id
  ; address_allocation_id
  ; association_status
  ; address_type
  ; service
  ; service_resource
  ; vpc_id
  ; subnet_id
  ; public_ipv4_pool_id
  ; network_interface_id
  ; network_interface_description
  ; instance_id
  ; tags
  ; network_border_group
  ; security_groups
  ; sample_time
  }

let parse xml =
  Some
    { ipam_resource_discovery_id =
        Aws.Util.option_bind (Aws.Xml.member "ipamResourceDiscoveryId" xml) String.parse
    ; address_region =
        Aws.Util.option_bind (Aws.Xml.member "addressRegion" xml) String.parse
    ; address = Aws.Util.option_bind (Aws.Xml.member "address" xml) String.parse
    ; address_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "addressOwnerId" xml) String.parse
    ; address_allocation_id =
        Aws.Util.option_bind (Aws.Xml.member "addressAllocationId" xml) String.parse
    ; association_status =
        Aws.Util.option_bind
          (Aws.Xml.member "associationStatus" xml)
          IpamPublicAddressAssociationStatus.parse
    ; address_type =
        Aws.Util.option_bind
          (Aws.Xml.member "addressType" xml)
          IpamPublicAddressType.parse
    ; service =
        Aws.Util.option_bind
          (Aws.Xml.member "service" xml)
          IpamPublicAddressAwsService.parse
    ; service_resource =
        Aws.Util.option_bind (Aws.Xml.member "serviceResource" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; public_ipv4_pool_id =
        Aws.Util.option_bind (Aws.Xml.member "publicIpv4PoolId" xml) String.parse
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; network_interface_description =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInterfaceDescription" xml)
          String.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; tags = Aws.Util.option_bind (Aws.Xml.member "tags" xml) IpamPublicAddressTags.parse
    ; network_border_group =
        Aws.Util.option_bind (Aws.Xml.member "networkBorderGroup" xml) String.parse
    ; security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "securityGroupSet" xml)
             IpamPublicAddressSecurityGroupList.parse)
    ; sample_time = Aws.Util.option_bind (Aws.Xml.member "sampleTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.sample_time (fun f ->
             Aws.Query.Pair ("SampleTime", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupSet"
              , IpamPublicAddressSecurityGroupList.to_query v.security_groups ))
       ; Aws.Util.option_map v.network_border_group (fun f ->
             Aws.Query.Pair ("NetworkBorderGroup", String.to_query f))
       ; Aws.Util.option_map v.tags (fun f ->
             Aws.Query.Pair ("Tags", IpamPublicAddressTags.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.network_interface_description (fun f ->
             Aws.Query.Pair ("NetworkInterfaceDescription", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.public_ipv4_pool_id (fun f ->
             Aws.Query.Pair ("PublicIpv4PoolId", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.service_resource (fun f ->
             Aws.Query.Pair ("ServiceResource", String.to_query f))
       ; Aws.Util.option_map v.service (fun f ->
             Aws.Query.Pair ("Service", IpamPublicAddressAwsService.to_query f))
       ; Aws.Util.option_map v.address_type (fun f ->
             Aws.Query.Pair ("AddressType", IpamPublicAddressType.to_query f))
       ; Aws.Util.option_map v.association_status (fun f ->
             Aws.Query.Pair
               ("AssociationStatus", IpamPublicAddressAssociationStatus.to_query f))
       ; Aws.Util.option_map v.address_allocation_id (fun f ->
             Aws.Query.Pair ("AddressAllocationId", String.to_query f))
       ; Aws.Util.option_map v.address_owner_id (fun f ->
             Aws.Query.Pair ("AddressOwnerId", String.to_query f))
       ; Aws.Util.option_map v.address (fun f ->
             Aws.Query.Pair ("Address", String.to_query f))
       ; Aws.Util.option_map v.address_region (fun f ->
             Aws.Query.Pair ("AddressRegion", String.to_query f))
       ; Aws.Util.option_map v.ipam_resource_discovery_id (fun f ->
             Aws.Query.Pair ("IpamResourceDiscoveryId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.sample_time (fun f -> "sampleTime", DateTime.to_json f)
       ; Some
           ( "securityGroupSet"
           , IpamPublicAddressSecurityGroupList.to_json v.security_groups )
       ; Aws.Util.option_map v.network_border_group (fun f ->
             "networkBorderGroup", String.to_json f)
       ; Aws.Util.option_map v.tags (fun f -> "tags", IpamPublicAddressTags.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.network_interface_description (fun f ->
             "networkInterfaceDescription", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.public_ipv4_pool_id (fun f ->
             "publicIpv4PoolId", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.service_resource (fun f ->
             "serviceResource", String.to_json f)
       ; Aws.Util.option_map v.service (fun f ->
             "service", IpamPublicAddressAwsService.to_json f)
       ; Aws.Util.option_map v.address_type (fun f ->
             "addressType", IpamPublicAddressType.to_json f)
       ; Aws.Util.option_map v.association_status (fun f ->
             "associationStatus", IpamPublicAddressAssociationStatus.to_json f)
       ; Aws.Util.option_map v.address_allocation_id (fun f ->
             "addressAllocationId", String.to_json f)
       ; Aws.Util.option_map v.address_owner_id (fun f ->
             "addressOwnerId", String.to_json f)
       ; Aws.Util.option_map v.address (fun f -> "address", String.to_json f)
       ; Aws.Util.option_map v.address_region (fun f -> "addressRegion", String.to_json f)
       ; Aws.Util.option_map v.ipam_resource_discovery_id (fun f ->
             "ipamResourceDiscoveryId", String.to_json f)
       ])

let of_json j =
  { ipam_resource_discovery_id =
      Aws.Util.option_map (Aws.Json.lookup j "ipamResourceDiscoveryId") String.of_json
  ; address_region =
      Aws.Util.option_map (Aws.Json.lookup j "addressRegion") String.of_json
  ; address = Aws.Util.option_map (Aws.Json.lookup j "address") String.of_json
  ; address_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "addressOwnerId") String.of_json
  ; address_allocation_id =
      Aws.Util.option_map (Aws.Json.lookup j "addressAllocationId") String.of_json
  ; association_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "associationStatus")
        IpamPublicAddressAssociationStatus.of_json
  ; address_type =
      Aws.Util.option_map (Aws.Json.lookup j "addressType") IpamPublicAddressType.of_json
  ; service =
      Aws.Util.option_map
        (Aws.Json.lookup j "service")
        IpamPublicAddressAwsService.of_json
  ; service_resource =
      Aws.Util.option_map (Aws.Json.lookup j "serviceResource") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; public_ipv4_pool_id =
      Aws.Util.option_map (Aws.Json.lookup j "publicIpv4PoolId") String.of_json
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; network_interface_description =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceDescription") String.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; tags = Aws.Util.option_map (Aws.Json.lookup j "tags") IpamPublicAddressTags.of_json
  ; network_border_group =
      Aws.Util.option_map (Aws.Json.lookup j "networkBorderGroup") String.of_json
  ; security_groups =
      IpamPublicAddressSecurityGroupList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "securityGroupSet"))
  ; sample_time = Aws.Util.option_map (Aws.Json.lookup j "sampleTime") DateTime.of_json
  }
