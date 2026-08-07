open Aws.BaseTypes

type t =
  { association : NetworkInterfaceAssociation.t option
  ; attachment : NetworkInterfaceAttachment.t option
  ; availability_zone : String.t option
  ; connection_tracking_configuration : ConnectionTrackingConfiguration.t option
  ; description : String.t option
  ; groups : GroupIdentifierList.t
  ; interface_type : NetworkInterfaceType.t option
  ; ipv6_addresses : NetworkInterfaceIpv6AddressesList.t
  ; mac_address : String.t option
  ; network_interface_id : String.t option
  ; outpost_arn : String.t option
  ; owner_id : String.t option
  ; private_dns_name : String.t option
  ; public_dns_name : String.t option
  ; public_ip_dns_name_options : PublicIpDnsNameOptions.t option
  ; private_ip_address : String.t option
  ; private_ip_addresses : NetworkInterfacePrivateIpAddressList.t
  ; ipv4_prefixes : Ipv4PrefixesList.t
  ; ipv6_prefixes : Ipv6PrefixesList.t
  ; requester_id : String.t option
  ; requester_managed : Boolean.t option
  ; source_dest_check : Boolean.t option
  ; status : NetworkInterfaceStatus.t option
  ; subnet_id : String.t option
  ; tag_set : TagList.t
  ; vpc_id : String.t option
  ; deny_all_igw_traffic : Boolean.t option
  ; ipv6_native : Boolean.t option
  ; ipv6_address : String.t option
  ; operator : OperatorResponse.t option
  ; associated_subnets : AssociatedSubnetList.t
  ; availability_zone_id : String.t option
  }

let make
    ?association
    ?attachment
    ?availability_zone
    ?connection_tracking_configuration
    ?description
    ?(groups = [])
    ?interface_type
    ?(ipv6_addresses = [])
    ?mac_address
    ?network_interface_id
    ?outpost_arn
    ?owner_id
    ?private_dns_name
    ?public_dns_name
    ?public_ip_dns_name_options
    ?private_ip_address
    ?(private_ip_addresses = [])
    ?(ipv4_prefixes = [])
    ?(ipv6_prefixes = [])
    ?requester_id
    ?requester_managed
    ?source_dest_check
    ?status
    ?subnet_id
    ?(tag_set = [])
    ?vpc_id
    ?deny_all_igw_traffic
    ?ipv6_native
    ?ipv6_address
    ?operator
    ?(associated_subnets = [])
    ?availability_zone_id
    () =
  { association
  ; attachment
  ; availability_zone
  ; connection_tracking_configuration
  ; description
  ; groups
  ; interface_type
  ; ipv6_addresses
  ; mac_address
  ; network_interface_id
  ; outpost_arn
  ; owner_id
  ; private_dns_name
  ; public_dns_name
  ; public_ip_dns_name_options
  ; private_ip_address
  ; private_ip_addresses
  ; ipv4_prefixes
  ; ipv6_prefixes
  ; requester_id
  ; requester_managed
  ; source_dest_check
  ; status
  ; subnet_id
  ; tag_set
  ; vpc_id
  ; deny_all_igw_traffic
  ; ipv6_native
  ; ipv6_address
  ; operator
  ; associated_subnets
  ; availability_zone_id
  }

let parse xml =
  Some
    { association =
        Aws.Util.option_bind
          (Aws.Xml.member "association" xml)
          NetworkInterfaceAssociation.parse
    ; attachment =
        Aws.Util.option_bind
          (Aws.Xml.member "attachment" xml)
          NetworkInterfaceAttachment.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; connection_tracking_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "connectionTrackingConfiguration" xml)
          ConnectionTrackingConfiguration.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "groupSet" xml) GroupIdentifierList.parse)
    ; interface_type =
        Aws.Util.option_bind
          (Aws.Xml.member "interfaceType" xml)
          NetworkInterfaceType.parse
    ; ipv6_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv6AddressesSet" xml)
             NetworkInterfaceIpv6AddressesList.parse)
    ; mac_address = Aws.Util.option_bind (Aws.Xml.member "macAddress" xml) String.parse
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; private_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "privateDnsName" xml) String.parse
    ; public_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "publicDnsName" xml) String.parse
    ; public_ip_dns_name_options =
        Aws.Util.option_bind
          (Aws.Xml.member "publicIpDnsNameOptions" xml)
          PublicIpDnsNameOptions.parse
    ; private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "privateIpAddress" xml) String.parse
    ; private_ip_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "privateIpAddressesSet" xml)
             NetworkInterfacePrivateIpAddressList.parse)
    ; ipv4_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv4PrefixSet" xml)
             Ipv4PrefixesList.parse)
    ; ipv6_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv6PrefixSet" xml)
             Ipv6PrefixesList.parse)
    ; requester_id = Aws.Util.option_bind (Aws.Xml.member "requesterId" xml) String.parse
    ; requester_managed =
        Aws.Util.option_bind (Aws.Xml.member "requesterManaged" xml) Boolean.parse
    ; source_dest_check =
        Aws.Util.option_bind (Aws.Xml.member "sourceDestCheck" xml) Boolean.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "status" xml) NetworkInterfaceStatus.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; tag_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; deny_all_igw_traffic =
        Aws.Util.option_bind (Aws.Xml.member "denyAllIgwTraffic" xml) Boolean.parse
    ; ipv6_native = Aws.Util.option_bind (Aws.Xml.member "ipv6Native" xml) Boolean.parse
    ; ipv6_address = Aws.Util.option_bind (Aws.Xml.member "ipv6Address" xml) String.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "operator" xml) OperatorResponse.parse
    ; associated_subnets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "associatedSubnetSet" xml)
             AssociatedSubnetList.parse)
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AssociatedSubnetSet", AssociatedSubnetList.to_query v.associated_subnets))
       ; Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorResponse.to_query f))
       ; Aws.Util.option_map v.ipv6_address (fun f ->
             Aws.Query.Pair ("Ipv6Address", String.to_query f))
       ; Aws.Util.option_map v.ipv6_native (fun f ->
             Aws.Query.Pair ("Ipv6Native", Boolean.to_query f))
       ; Aws.Util.option_map v.deny_all_igw_traffic (fun f ->
             Aws.Query.Pair ("DenyAllIgwTraffic", Boolean.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tag_set))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", NetworkInterfaceStatus.to_query f))
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             Aws.Query.Pair ("SourceDestCheck", Boolean.to_query f))
       ; Aws.Util.option_map v.requester_managed (fun f ->
             Aws.Query.Pair ("RequesterManaged", Boolean.to_query f))
       ; Aws.Util.option_map v.requester_id (fun f ->
             Aws.Query.Pair ("RequesterId", String.to_query f))
       ; Some
           (Aws.Query.Pair ("Ipv6PrefixSet", Ipv6PrefixesList.to_query v.ipv6_prefixes))
       ; Some
           (Aws.Query.Pair ("Ipv4PrefixSet", Ipv4PrefixesList.to_query v.ipv4_prefixes))
       ; Some
           (Aws.Query.Pair
              ( "PrivateIpAddressesSet"
              , NetworkInterfacePrivateIpAddressList.to_query v.private_ip_addresses ))
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             Aws.Query.Pair ("PrivateIpAddress", String.to_query f))
       ; Aws.Util.option_map v.public_ip_dns_name_options (fun f ->
             Aws.Query.Pair ("PublicIpDnsNameOptions", PublicIpDnsNameOptions.to_query f))
       ; Aws.Util.option_map v.public_dns_name (fun f ->
             Aws.Query.Pair ("PublicDnsName", String.to_query f))
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             Aws.Query.Pair ("PrivateDnsName", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.mac_address (fun f ->
             Aws.Query.Pair ("MacAddress", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "Ipv6AddressesSet"
              , NetworkInterfaceIpv6AddressesList.to_query v.ipv6_addresses ))
       ; Aws.Util.option_map v.interface_type (fun f ->
             Aws.Query.Pair ("InterfaceType", NetworkInterfaceType.to_query f))
       ; Some (Aws.Query.Pair ("GroupSet", GroupIdentifierList.to_query v.groups))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.connection_tracking_configuration (fun f ->
             Aws.Query.Pair
               ( "ConnectionTrackingConfiguration"
               , ConnectionTrackingConfiguration.to_query f ))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.attachment (fun f ->
             Aws.Query.Pair ("Attachment", NetworkInterfaceAttachment.to_query f))
       ; Aws.Util.option_map v.association (fun f ->
             Aws.Query.Pair ("Association", NetworkInterfaceAssociation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Some ("associatedSubnetSet", AssociatedSubnetList.to_json v.associated_subnets)
       ; Aws.Util.option_map v.operator (fun f -> "operator", OperatorResponse.to_json f)
       ; Aws.Util.option_map v.ipv6_address (fun f -> "ipv6Address", String.to_json f)
       ; Aws.Util.option_map v.ipv6_native (fun f -> "ipv6Native", Boolean.to_json f)
       ; Aws.Util.option_map v.deny_all_igw_traffic (fun f ->
             "denyAllIgwTraffic", Boolean.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tag_set)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "status", NetworkInterfaceStatus.to_json f)
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             "sourceDestCheck", Boolean.to_json f)
       ; Aws.Util.option_map v.requester_managed (fun f ->
             "requesterManaged", Boolean.to_json f)
       ; Aws.Util.option_map v.requester_id (fun f -> "requesterId", String.to_json f)
       ; Some ("ipv6PrefixSet", Ipv6PrefixesList.to_json v.ipv6_prefixes)
       ; Some ("ipv4PrefixSet", Ipv4PrefixesList.to_json v.ipv4_prefixes)
       ; Some
           ( "privateIpAddressesSet"
           , NetworkInterfacePrivateIpAddressList.to_json v.private_ip_addresses )
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             "privateIpAddress", String.to_json f)
       ; Aws.Util.option_map v.public_ip_dns_name_options (fun f ->
             "publicIpDnsNameOptions", PublicIpDnsNameOptions.to_json f)
       ; Aws.Util.option_map v.public_dns_name (fun f ->
             "publicDnsName", String.to_json f)
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             "privateDnsName", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.mac_address (fun f -> "macAddress", String.to_json f)
       ; Some
           ("ipv6AddressesSet", NetworkInterfaceIpv6AddressesList.to_json v.ipv6_addresses)
       ; Aws.Util.option_map v.interface_type (fun f ->
             "interfaceType", NetworkInterfaceType.to_json f)
       ; Some ("groupSet", GroupIdentifierList.to_json v.groups)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.connection_tracking_configuration (fun f ->
             "connectionTrackingConfiguration", ConnectionTrackingConfiguration.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.attachment (fun f ->
             "attachment", NetworkInterfaceAttachment.to_json f)
       ; Aws.Util.option_map v.association (fun f ->
             "association", NetworkInterfaceAssociation.to_json f)
       ])

let of_json j =
  { association =
      Aws.Util.option_map
        (Aws.Json.lookup j "association")
        NetworkInterfaceAssociation.of_json
  ; attachment =
      Aws.Util.option_map
        (Aws.Json.lookup j "attachment")
        NetworkInterfaceAttachment.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; connection_tracking_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "connectionTrackingConfiguration")
        ConnectionTrackingConfiguration.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; groups =
      GroupIdentifierList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupSet"))
  ; interface_type =
      Aws.Util.option_map (Aws.Json.lookup j "interfaceType") NetworkInterfaceType.of_json
  ; ipv6_addresses =
      NetworkInterfaceIpv6AddressesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6AddressesSet"))
  ; mac_address = Aws.Util.option_map (Aws.Json.lookup j "macAddress") String.of_json
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; private_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "privateDnsName") String.of_json
  ; public_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "publicDnsName") String.of_json
  ; public_ip_dns_name_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "publicIpDnsNameOptions")
        PublicIpDnsNameOptions.of_json
  ; private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "privateIpAddress") String.of_json
  ; private_ip_addresses =
      NetworkInterfacePrivateIpAddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "privateIpAddressesSet"))
  ; ipv4_prefixes =
      Ipv4PrefixesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv4PrefixSet"))
  ; ipv6_prefixes =
      Ipv6PrefixesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6PrefixSet"))
  ; requester_id = Aws.Util.option_map (Aws.Json.lookup j "requesterId") String.of_json
  ; requester_managed =
      Aws.Util.option_map (Aws.Json.lookup j "requesterManaged") Boolean.of_json
  ; source_dest_check =
      Aws.Util.option_map (Aws.Json.lookup j "sourceDestCheck") Boolean.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "status") NetworkInterfaceStatus.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; tag_set = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; deny_all_igw_traffic =
      Aws.Util.option_map (Aws.Json.lookup j "denyAllIgwTraffic") Boolean.of_json
  ; ipv6_native = Aws.Util.option_map (Aws.Json.lookup j "ipv6Native") Boolean.of_json
  ; ipv6_address = Aws.Util.option_map (Aws.Json.lookup j "ipv6Address") String.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "operator") OperatorResponse.of_json
  ; associated_subnets =
      AssociatedSubnetList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "associatedSubnetSet"))
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  }
