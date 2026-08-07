open Aws.BaseTypes

type t =
  { associate_public_ip_address : Boolean.t option
  ; delete_on_termination : Boolean.t option
  ; description : String.t option
  ; device_index : Integer.t option
  ; groups : ScheduledInstancesSecurityGroupIdSet.t
  ; ipv6_address_count : Integer.t option
  ; ipv6_addresses : ScheduledInstancesIpv6AddressList.t
  ; network_interface_id : String.t option
  ; private_ip_address : String.t option
  ; private_ip_address_configs : PrivateIpAddressConfigSet.t
  ; secondary_private_ip_address_count : Integer.t option
  ; subnet_id : String.t option
  }

let make
    ?associate_public_ip_address
    ?delete_on_termination
    ?description
    ?device_index
    ?(groups = [])
    ?ipv6_address_count
    ?(ipv6_addresses = [])
    ?network_interface_id
    ?private_ip_address
    ?(private_ip_address_configs = [])
    ?secondary_private_ip_address_count
    ?subnet_id
    () =
  { associate_public_ip_address
  ; delete_on_termination
  ; description
  ; device_index
  ; groups
  ; ipv6_address_count
  ; ipv6_addresses
  ; network_interface_id
  ; private_ip_address
  ; private_ip_address_configs
  ; secondary_private_ip_address_count
  ; subnet_id
  }

let parse xml =
  Some
    { associate_public_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "AssociatePublicIpAddress" xml) Boolean.parse
    ; delete_on_termination =
        Aws.Util.option_bind (Aws.Xml.member "DeleteOnTermination" xml) Boolean.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; device_index = Aws.Util.option_bind (Aws.Xml.member "DeviceIndex" xml) Integer.parse
    ; groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Group" xml)
             ScheduledInstancesSecurityGroupIdSet.parse)
    ; ipv6_address_count =
        Aws.Util.option_bind (Aws.Xml.member "Ipv6AddressCount" xml) Integer.parse
    ; ipv6_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Ipv6Address" xml)
             ScheduledInstancesIpv6AddressList.parse)
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "NetworkInterfaceId" xml) String.parse
    ; private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "PrivateIpAddress" xml) String.parse
    ; private_ip_address_configs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PrivateIpAddressConfig" xml)
             PrivateIpAddressConfigSet.parse)
    ; secondary_private_ip_address_count =
        Aws.Util.option_bind
          (Aws.Xml.member "SecondaryPrivateIpAddressCount" xml)
          Integer.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "SubnetId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.secondary_private_ip_address_count (fun f ->
             Aws.Query.Pair ("SecondaryPrivateIpAddressCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PrivateIpAddressConfig"
              , PrivateIpAddressConfigSet.to_query v.private_ip_address_configs ))
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             Aws.Query.Pair ("PrivateIpAddress", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Ipv6Address", ScheduledInstancesIpv6AddressList.to_query v.ipv6_addresses))
       ; Aws.Util.option_map v.ipv6_address_count (fun f ->
             Aws.Query.Pair ("Ipv6AddressCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Group", ScheduledInstancesSecurityGroupIdSet.to_query v.groups))
       ; Aws.Util.option_map v.device_index (fun f ->
             Aws.Query.Pair ("DeviceIndex", Integer.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             Aws.Query.Pair ("DeleteOnTermination", Boolean.to_query f))
       ; Aws.Util.option_map v.associate_public_ip_address (fun f ->
             Aws.Query.Pair ("AssociatePublicIpAddress", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subnet_id (fun f -> "SubnetId", String.to_json f)
       ; Aws.Util.option_map v.secondary_private_ip_address_count (fun f ->
             "SecondaryPrivateIpAddressCount", Integer.to_json f)
       ; Some
           ( "PrivateIpAddressConfig"
           , PrivateIpAddressConfigSet.to_json v.private_ip_address_configs )
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             "PrivateIpAddress", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "NetworkInterfaceId", String.to_json f)
       ; Some ("Ipv6Address", ScheduledInstancesIpv6AddressList.to_json v.ipv6_addresses)
       ; Aws.Util.option_map v.ipv6_address_count (fun f ->
             "Ipv6AddressCount", Integer.to_json f)
       ; Some ("Group", ScheduledInstancesSecurityGroupIdSet.to_json v.groups)
       ; Aws.Util.option_map v.device_index (fun f -> "DeviceIndex", Integer.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             "DeleteOnTermination", Boolean.to_json f)
       ; Aws.Util.option_map v.associate_public_ip_address (fun f ->
             "AssociatePublicIpAddress", Boolean.to_json f)
       ])

let of_json j =
  { associate_public_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "AssociatePublicIpAddress") Boolean.of_json
  ; delete_on_termination =
      Aws.Util.option_map (Aws.Json.lookup j "DeleteOnTermination") Boolean.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; device_index = Aws.Util.option_map (Aws.Json.lookup j "DeviceIndex") Integer.of_json
  ; groups =
      ScheduledInstancesSecurityGroupIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Group"))
  ; ipv6_address_count =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv6AddressCount") Integer.of_json
  ; ipv6_addresses =
      ScheduledInstancesIpv6AddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Ipv6Address"))
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkInterfaceId") String.of_json
  ; private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "PrivateIpAddress") String.of_json
  ; private_ip_address_configs =
      PrivateIpAddressConfigSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PrivateIpAddressConfig"))
  ; secondary_private_ip_address_count =
      Aws.Util.option_map
        (Aws.Json.lookup j "SecondaryPrivateIpAddressCount")
        Integer.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "SubnetId") String.of_json
  }
