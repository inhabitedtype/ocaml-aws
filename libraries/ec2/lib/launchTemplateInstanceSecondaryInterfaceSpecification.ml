open Aws.BaseTypes

type t =
  { delete_on_termination : Boolean.t option
  ; device_index : Integer.t option
  ; private_ip_addresses : SecondaryInterfacePrivateIpAddressSpecificationList.t
  ; private_ip_address_count : Integer.t option
  ; secondary_subnet_id : String.t option
  ; interface_type : SecondaryInterfaceType.t option
  ; network_card_index : Integer.t option
  }

let make
    ?delete_on_termination
    ?device_index
    ?(private_ip_addresses = [])
    ?private_ip_address_count
    ?secondary_subnet_id
    ?interface_type
    ?network_card_index
    () =
  { delete_on_termination
  ; device_index
  ; private_ip_addresses
  ; private_ip_address_count
  ; secondary_subnet_id
  ; interface_type
  ; network_card_index
  }

let parse xml =
  Some
    { delete_on_termination =
        Aws.Util.option_bind (Aws.Xml.member "deleteOnTermination" xml) Boolean.parse
    ; device_index = Aws.Util.option_bind (Aws.Xml.member "deviceIndex" xml) Integer.parse
    ; private_ip_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "privateIpAddressesSet" xml)
             SecondaryInterfacePrivateIpAddressSpecificationList.parse)
    ; private_ip_address_count =
        Aws.Util.option_bind (Aws.Xml.member "privateIpAddressCount" xml) Integer.parse
    ; secondary_subnet_id =
        Aws.Util.option_bind (Aws.Xml.member "secondarySubnetId" xml) String.parse
    ; interface_type =
        Aws.Util.option_bind
          (Aws.Xml.member "interfaceType" xml)
          SecondaryInterfaceType.parse
    ; network_card_index =
        Aws.Util.option_bind (Aws.Xml.member "networkCardIndex" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_card_index (fun f ->
             Aws.Query.Pair ("NetworkCardIndex", Integer.to_query f))
       ; Aws.Util.option_map v.interface_type (fun f ->
             Aws.Query.Pair ("InterfaceType", SecondaryInterfaceType.to_query f))
       ; Aws.Util.option_map v.secondary_subnet_id (fun f ->
             Aws.Query.Pair ("SecondarySubnetId", String.to_query f))
       ; Aws.Util.option_map v.private_ip_address_count (fun f ->
             Aws.Query.Pair ("PrivateIpAddressCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PrivateIpAddressesSet"
              , SecondaryInterfacePrivateIpAddressSpecificationList.to_query
                  v.private_ip_addresses ))
       ; Aws.Util.option_map v.device_index (fun f ->
             Aws.Query.Pair ("DeviceIndex", Integer.to_query f))
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             Aws.Query.Pair ("DeleteOnTermination", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_card_index (fun f ->
             "networkCardIndex", Integer.to_json f)
       ; Aws.Util.option_map v.interface_type (fun f ->
             "interfaceType", SecondaryInterfaceType.to_json f)
       ; Aws.Util.option_map v.secondary_subnet_id (fun f ->
             "secondarySubnetId", String.to_json f)
       ; Aws.Util.option_map v.private_ip_address_count (fun f ->
             "privateIpAddressCount", Integer.to_json f)
       ; Some
           ( "privateIpAddressesSet"
           , SecondaryInterfacePrivateIpAddressSpecificationList.to_json
               v.private_ip_addresses )
       ; Aws.Util.option_map v.device_index (fun f -> "deviceIndex", Integer.to_json f)
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             "deleteOnTermination", Boolean.to_json f)
       ])

let of_json j =
  { delete_on_termination =
      Aws.Util.option_map (Aws.Json.lookup j "deleteOnTermination") Boolean.of_json
  ; device_index = Aws.Util.option_map (Aws.Json.lookup j "deviceIndex") Integer.of_json
  ; private_ip_addresses =
      SecondaryInterfacePrivateIpAddressSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "privateIpAddressesSet"))
  ; private_ip_address_count =
      Aws.Util.option_map (Aws.Json.lookup j "privateIpAddressCount") Integer.of_json
  ; secondary_subnet_id =
      Aws.Util.option_map (Aws.Json.lookup j "secondarySubnetId") String.of_json
  ; interface_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "interfaceType")
        SecondaryInterfaceType.of_json
  ; network_card_index =
      Aws.Util.option_map (Aws.Json.lookup j "networkCardIndex") Integer.of_json
  }
