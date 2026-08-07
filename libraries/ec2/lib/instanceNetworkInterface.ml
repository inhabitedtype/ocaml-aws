open Aws.BaseTypes

type t =
  { association : InstanceNetworkInterfaceAssociation.t option
  ; attachment : InstanceNetworkInterfaceAttachment.t option
  ; description : String.t option
  ; groups : GroupIdentifierList.t
  ; ipv6_addresses : InstanceIpv6AddressList.t
  ; mac_address : String.t option
  ; network_interface_id : String.t option
  ; owner_id : String.t option
  ; private_dns_name : String.t option
  ; private_ip_address : String.t option
  ; private_ip_addresses : InstancePrivateIpAddressList.t
  ; source_dest_check : Boolean.t option
  ; status : NetworkInterfaceStatus.t option
  ; subnet_id : String.t option
  ; vpc_id : String.t option
  ; interface_type : String.t option
  ; ipv4_prefixes : InstanceIpv4PrefixList.t
  ; ipv6_prefixes : InstanceIpv6PrefixList.t
  ; connection_tracking_configuration : ConnectionTrackingSpecificationResponse.t option
  ; operator : OperatorResponse.t option
  }

let make
    ?association
    ?attachment
    ?description
    ?(groups = [])
    ?(ipv6_addresses = [])
    ?mac_address
    ?network_interface_id
    ?owner_id
    ?private_dns_name
    ?private_ip_address
    ?(private_ip_addresses = [])
    ?source_dest_check
    ?status
    ?subnet_id
    ?vpc_id
    ?interface_type
    ?(ipv4_prefixes = [])
    ?(ipv6_prefixes = [])
    ?connection_tracking_configuration
    ?operator
    () =
  { association
  ; attachment
  ; description
  ; groups
  ; ipv6_addresses
  ; mac_address
  ; network_interface_id
  ; owner_id
  ; private_dns_name
  ; private_ip_address
  ; private_ip_addresses
  ; source_dest_check
  ; status
  ; subnet_id
  ; vpc_id
  ; interface_type
  ; ipv4_prefixes
  ; ipv6_prefixes
  ; connection_tracking_configuration
  ; operator
  }

let parse xml =
  Some
    { association =
        Aws.Util.option_bind
          (Aws.Xml.member "association" xml)
          InstanceNetworkInterfaceAssociation.parse
    ; attachment =
        Aws.Util.option_bind
          (Aws.Xml.member "attachment" xml)
          InstanceNetworkInterfaceAttachment.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "groupSet" xml) GroupIdentifierList.parse)
    ; ipv6_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv6AddressesSet" xml)
             InstanceIpv6AddressList.parse)
    ; mac_address = Aws.Util.option_bind (Aws.Xml.member "macAddress" xml) String.parse
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; private_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "privateDnsName" xml) String.parse
    ; private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "privateIpAddress" xml) String.parse
    ; private_ip_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "privateIpAddressesSet" xml)
             InstancePrivateIpAddressList.parse)
    ; source_dest_check =
        Aws.Util.option_bind (Aws.Xml.member "sourceDestCheck" xml) Boolean.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "status" xml) NetworkInterfaceStatus.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; interface_type =
        Aws.Util.option_bind (Aws.Xml.member "interfaceType" xml) String.parse
    ; ipv4_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv4PrefixSet" xml)
             InstanceIpv4PrefixList.parse)
    ; ipv6_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv6PrefixSet" xml)
             InstanceIpv6PrefixList.parse)
    ; connection_tracking_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "connectionTrackingConfiguration" xml)
          ConnectionTrackingSpecificationResponse.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "operator" xml) OperatorResponse.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorResponse.to_query f))
       ; Aws.Util.option_map v.connection_tracking_configuration (fun f ->
             Aws.Query.Pair
               ( "ConnectionTrackingConfiguration"
               , ConnectionTrackingSpecificationResponse.to_query f ))
       ; Some
           (Aws.Query.Pair
              ("Ipv6PrefixSet", InstanceIpv6PrefixList.to_query v.ipv6_prefixes))
       ; Some
           (Aws.Query.Pair
              ("Ipv4PrefixSet", InstanceIpv4PrefixList.to_query v.ipv4_prefixes))
       ; Aws.Util.option_map v.interface_type (fun f ->
             Aws.Query.Pair ("InterfaceType", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", NetworkInterfaceStatus.to_query f))
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             Aws.Query.Pair ("SourceDestCheck", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PrivateIpAddressesSet"
              , InstancePrivateIpAddressList.to_query v.private_ip_addresses ))
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             Aws.Query.Pair ("PrivateIpAddress", String.to_query f))
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             Aws.Query.Pair ("PrivateDnsName", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.mac_address (fun f ->
             Aws.Query.Pair ("MacAddress", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Ipv6AddressesSet", InstanceIpv6AddressList.to_query v.ipv6_addresses))
       ; Some (Aws.Query.Pair ("GroupSet", GroupIdentifierList.to_query v.groups))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.attachment (fun f ->
             Aws.Query.Pair ("Attachment", InstanceNetworkInterfaceAttachment.to_query f))
       ; Aws.Util.option_map v.association (fun f ->
             Aws.Query.Pair ("Association", InstanceNetworkInterfaceAssociation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operator (fun f -> "operator", OperatorResponse.to_json f)
       ; Aws.Util.option_map v.connection_tracking_configuration (fun f ->
             ( "connectionTrackingConfiguration"
             , ConnectionTrackingSpecificationResponse.to_json f ))
       ; Some ("ipv6PrefixSet", InstanceIpv6PrefixList.to_json v.ipv6_prefixes)
       ; Some ("ipv4PrefixSet", InstanceIpv4PrefixList.to_json v.ipv4_prefixes)
       ; Aws.Util.option_map v.interface_type (fun f -> "interfaceType", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "status", NetworkInterfaceStatus.to_json f)
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             "sourceDestCheck", Boolean.to_json f)
       ; Some
           ( "privateIpAddressesSet"
           , InstancePrivateIpAddressList.to_json v.private_ip_addresses )
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             "privateIpAddress", String.to_json f)
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             "privateDnsName", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.mac_address (fun f -> "macAddress", String.to_json f)
       ; Some ("ipv6AddressesSet", InstanceIpv6AddressList.to_json v.ipv6_addresses)
       ; Some ("groupSet", GroupIdentifierList.to_json v.groups)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.attachment (fun f ->
             "attachment", InstanceNetworkInterfaceAttachment.to_json f)
       ; Aws.Util.option_map v.association (fun f ->
             "association", InstanceNetworkInterfaceAssociation.to_json f)
       ])

let of_json j =
  { association =
      Aws.Util.option_map
        (Aws.Json.lookup j "association")
        InstanceNetworkInterfaceAssociation.of_json
  ; attachment =
      Aws.Util.option_map
        (Aws.Json.lookup j "attachment")
        InstanceNetworkInterfaceAttachment.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; groups =
      GroupIdentifierList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupSet"))
  ; ipv6_addresses =
      InstanceIpv6AddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6AddressesSet"))
  ; mac_address = Aws.Util.option_map (Aws.Json.lookup j "macAddress") String.of_json
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; private_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "privateDnsName") String.of_json
  ; private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "privateIpAddress") String.of_json
  ; private_ip_addresses =
      InstancePrivateIpAddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "privateIpAddressesSet"))
  ; source_dest_check =
      Aws.Util.option_map (Aws.Json.lookup j "sourceDestCheck") Boolean.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "status") NetworkInterfaceStatus.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; interface_type =
      Aws.Util.option_map (Aws.Json.lookup j "interfaceType") String.of_json
  ; ipv4_prefixes =
      InstanceIpv4PrefixList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv4PrefixSet"))
  ; ipv6_prefixes =
      InstanceIpv6PrefixList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6PrefixSet"))
  ; connection_tracking_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "connectionTrackingConfiguration")
        ConnectionTrackingSpecificationResponse.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "operator") OperatorResponse.of_json
  }
