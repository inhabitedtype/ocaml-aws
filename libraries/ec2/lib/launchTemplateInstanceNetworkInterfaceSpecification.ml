open Aws.BaseTypes

type t =
  { associate_carrier_ip_address : Boolean.t option
  ; associate_public_ip_address : Boolean.t option
  ; delete_on_termination : Boolean.t option
  ; description : String.t option
  ; device_index : Integer.t option
  ; groups : GroupIdStringList.t
  ; interface_type : String.t option
  ; ipv6_address_count : Integer.t option
  ; ipv6_addresses : InstanceIpv6AddressList.t
  ; network_interface_id : String.t option
  ; private_ip_address : String.t option
  ; private_ip_addresses : PrivateIpAddressSpecificationList.t
  ; secondary_private_ip_address_count : Integer.t option
  ; subnet_id : String.t option
  ; network_card_index : Integer.t option
  ; ipv4_prefixes : Ipv4PrefixListResponse.t
  ; ipv4_prefix_count : Integer.t option
  ; ipv6_prefixes : Ipv6PrefixListResponse.t
  ; ipv6_prefix_count : Integer.t option
  ; primary_ipv6 : Boolean.t option
  ; ena_srd_specification : LaunchTemplateEnaSrdSpecification.t option
  ; connection_tracking_specification : ConnectionTrackingSpecification.t option
  ; ena_queue_count : Integer.t option
  }

let make
    ?associate_carrier_ip_address
    ?associate_public_ip_address
    ?delete_on_termination
    ?description
    ?device_index
    ?(groups = [])
    ?interface_type
    ?ipv6_address_count
    ?(ipv6_addresses = [])
    ?network_interface_id
    ?private_ip_address
    ?(private_ip_addresses = [])
    ?secondary_private_ip_address_count
    ?subnet_id
    ?network_card_index
    ?(ipv4_prefixes = [])
    ?ipv4_prefix_count
    ?(ipv6_prefixes = [])
    ?ipv6_prefix_count
    ?primary_ipv6
    ?ena_srd_specification
    ?connection_tracking_specification
    ?ena_queue_count
    () =
  { associate_carrier_ip_address
  ; associate_public_ip_address
  ; delete_on_termination
  ; description
  ; device_index
  ; groups
  ; interface_type
  ; ipv6_address_count
  ; ipv6_addresses
  ; network_interface_id
  ; private_ip_address
  ; private_ip_addresses
  ; secondary_private_ip_address_count
  ; subnet_id
  ; network_card_index
  ; ipv4_prefixes
  ; ipv4_prefix_count
  ; ipv6_prefixes
  ; ipv6_prefix_count
  ; primary_ipv6
  ; ena_srd_specification
  ; connection_tracking_specification
  ; ena_queue_count
  }

let parse xml =
  Some
    { associate_carrier_ip_address =
        Aws.Util.option_bind
          (Aws.Xml.member "associateCarrierIpAddress" xml)
          Boolean.parse
    ; associate_public_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "associatePublicIpAddress" xml) Boolean.parse
    ; delete_on_termination =
        Aws.Util.option_bind (Aws.Xml.member "deleteOnTermination" xml) Boolean.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; device_index = Aws.Util.option_bind (Aws.Xml.member "deviceIndex" xml) Integer.parse
    ; groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "groupSet" xml) GroupIdStringList.parse)
    ; interface_type =
        Aws.Util.option_bind (Aws.Xml.member "interfaceType" xml) String.parse
    ; ipv6_address_count =
        Aws.Util.option_bind (Aws.Xml.member "ipv6AddressCount" xml) Integer.parse
    ; ipv6_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv6AddressesSet" xml)
             InstanceIpv6AddressList.parse)
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "privateIpAddress" xml) String.parse
    ; private_ip_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "privateIpAddressesSet" xml)
             PrivateIpAddressSpecificationList.parse)
    ; secondary_private_ip_address_count =
        Aws.Util.option_bind
          (Aws.Xml.member "secondaryPrivateIpAddressCount" xml)
          Integer.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; network_card_index =
        Aws.Util.option_bind (Aws.Xml.member "networkCardIndex" xml) Integer.parse
    ; ipv4_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv4PrefixSet" xml)
             Ipv4PrefixListResponse.parse)
    ; ipv4_prefix_count =
        Aws.Util.option_bind (Aws.Xml.member "ipv4PrefixCount" xml) Integer.parse
    ; ipv6_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv6PrefixSet" xml)
             Ipv6PrefixListResponse.parse)
    ; ipv6_prefix_count =
        Aws.Util.option_bind (Aws.Xml.member "ipv6PrefixCount" xml) Integer.parse
    ; primary_ipv6 = Aws.Util.option_bind (Aws.Xml.member "primaryIpv6" xml) Boolean.parse
    ; ena_srd_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "enaSrdSpecification" xml)
          LaunchTemplateEnaSrdSpecification.parse
    ; connection_tracking_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "connectionTrackingSpecification" xml)
          ConnectionTrackingSpecification.parse
    ; ena_queue_count =
        Aws.Util.option_bind (Aws.Xml.member "enaQueueCount" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ena_queue_count (fun f ->
             Aws.Query.Pair ("EnaQueueCount", Integer.to_query f))
       ; Aws.Util.option_map v.connection_tracking_specification (fun f ->
             Aws.Query.Pair
               ( "ConnectionTrackingSpecification"
               , ConnectionTrackingSpecification.to_query f ))
       ; Aws.Util.option_map v.ena_srd_specification (fun f ->
             Aws.Query.Pair
               ("EnaSrdSpecification", LaunchTemplateEnaSrdSpecification.to_query f))
       ; Aws.Util.option_map v.primary_ipv6 (fun f ->
             Aws.Query.Pair ("PrimaryIpv6", Boolean.to_query f))
       ; Aws.Util.option_map v.ipv6_prefix_count (fun f ->
             Aws.Query.Pair ("Ipv6PrefixCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Ipv6PrefixSet", Ipv6PrefixListResponse.to_query v.ipv6_prefixes))
       ; Aws.Util.option_map v.ipv4_prefix_count (fun f ->
             Aws.Query.Pair ("Ipv4PrefixCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Ipv4PrefixSet", Ipv4PrefixListResponse.to_query v.ipv4_prefixes))
       ; Aws.Util.option_map v.network_card_index (fun f ->
             Aws.Query.Pair ("NetworkCardIndex", Integer.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.secondary_private_ip_address_count (fun f ->
             Aws.Query.Pair ("SecondaryPrivateIpAddressCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PrivateIpAddressesSet"
              , PrivateIpAddressSpecificationList.to_query v.private_ip_addresses ))
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             Aws.Query.Pair ("PrivateIpAddress", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Ipv6AddressesSet", InstanceIpv6AddressList.to_query v.ipv6_addresses))
       ; Aws.Util.option_map v.ipv6_address_count (fun f ->
             Aws.Query.Pair ("Ipv6AddressCount", Integer.to_query f))
       ; Aws.Util.option_map v.interface_type (fun f ->
             Aws.Query.Pair ("InterfaceType", String.to_query f))
       ; Some (Aws.Query.Pair ("GroupSet", GroupIdStringList.to_query v.groups))
       ; Aws.Util.option_map v.device_index (fun f ->
             Aws.Query.Pair ("DeviceIndex", Integer.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             Aws.Query.Pair ("DeleteOnTermination", Boolean.to_query f))
       ; Aws.Util.option_map v.associate_public_ip_address (fun f ->
             Aws.Query.Pair ("AssociatePublicIpAddress", Boolean.to_query f))
       ; Aws.Util.option_map v.associate_carrier_ip_address (fun f ->
             Aws.Query.Pair ("AssociateCarrierIpAddress", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ena_queue_count (fun f ->
             "enaQueueCount", Integer.to_json f)
       ; Aws.Util.option_map v.connection_tracking_specification (fun f ->
             "connectionTrackingSpecification", ConnectionTrackingSpecification.to_json f)
       ; Aws.Util.option_map v.ena_srd_specification (fun f ->
             "enaSrdSpecification", LaunchTemplateEnaSrdSpecification.to_json f)
       ; Aws.Util.option_map v.primary_ipv6 (fun f -> "primaryIpv6", Boolean.to_json f)
       ; Aws.Util.option_map v.ipv6_prefix_count (fun f ->
             "ipv6PrefixCount", Integer.to_json f)
       ; Some ("ipv6PrefixSet", Ipv6PrefixListResponse.to_json v.ipv6_prefixes)
       ; Aws.Util.option_map v.ipv4_prefix_count (fun f ->
             "ipv4PrefixCount", Integer.to_json f)
       ; Some ("ipv4PrefixSet", Ipv4PrefixListResponse.to_json v.ipv4_prefixes)
       ; Aws.Util.option_map v.network_card_index (fun f ->
             "networkCardIndex", Integer.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.secondary_private_ip_address_count (fun f ->
             "secondaryPrivateIpAddressCount", Integer.to_json f)
       ; Some
           ( "privateIpAddressesSet"
           , PrivateIpAddressSpecificationList.to_json v.private_ip_addresses )
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             "privateIpAddress", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Some ("ipv6AddressesSet", InstanceIpv6AddressList.to_json v.ipv6_addresses)
       ; Aws.Util.option_map v.ipv6_address_count (fun f ->
             "ipv6AddressCount", Integer.to_json f)
       ; Aws.Util.option_map v.interface_type (fun f -> "interfaceType", String.to_json f)
       ; Some ("groupSet", GroupIdStringList.to_json v.groups)
       ; Aws.Util.option_map v.device_index (fun f -> "deviceIndex", Integer.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             "deleteOnTermination", Boolean.to_json f)
       ; Aws.Util.option_map v.associate_public_ip_address (fun f ->
             "associatePublicIpAddress", Boolean.to_json f)
       ; Aws.Util.option_map v.associate_carrier_ip_address (fun f ->
             "associateCarrierIpAddress", Boolean.to_json f)
       ])

let of_json j =
  { associate_carrier_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "associateCarrierIpAddress") Boolean.of_json
  ; associate_public_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "associatePublicIpAddress") Boolean.of_json
  ; delete_on_termination =
      Aws.Util.option_map (Aws.Json.lookup j "deleteOnTermination") Boolean.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; device_index = Aws.Util.option_map (Aws.Json.lookup j "deviceIndex") Integer.of_json
  ; groups =
      GroupIdStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupSet"))
  ; interface_type =
      Aws.Util.option_map (Aws.Json.lookup j "interfaceType") String.of_json
  ; ipv6_address_count =
      Aws.Util.option_map (Aws.Json.lookup j "ipv6AddressCount") Integer.of_json
  ; ipv6_addresses =
      InstanceIpv6AddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6AddressesSet"))
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "privateIpAddress") String.of_json
  ; private_ip_addresses =
      PrivateIpAddressSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "privateIpAddressesSet"))
  ; secondary_private_ip_address_count =
      Aws.Util.option_map
        (Aws.Json.lookup j "secondaryPrivateIpAddressCount")
        Integer.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; network_card_index =
      Aws.Util.option_map (Aws.Json.lookup j "networkCardIndex") Integer.of_json
  ; ipv4_prefixes =
      Ipv4PrefixListResponse.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv4PrefixSet"))
  ; ipv4_prefix_count =
      Aws.Util.option_map (Aws.Json.lookup j "ipv4PrefixCount") Integer.of_json
  ; ipv6_prefixes =
      Ipv6PrefixListResponse.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6PrefixSet"))
  ; ipv6_prefix_count =
      Aws.Util.option_map (Aws.Json.lookup j "ipv6PrefixCount") Integer.of_json
  ; primary_ipv6 = Aws.Util.option_map (Aws.Json.lookup j "primaryIpv6") Boolean.of_json
  ; ena_srd_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "enaSrdSpecification")
        LaunchTemplateEnaSrdSpecification.of_json
  ; connection_tracking_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "connectionTrackingSpecification")
        ConnectionTrackingSpecification.of_json
  ; ena_queue_count =
      Aws.Util.option_map (Aws.Json.lookup j "enaQueueCount") Integer.of_json
  }
