open Aws.BaseTypes

type t =
  { attachment : InstanceSecondaryInterfaceAttachment.t option
  ; mac_address : String.t option
  ; secondary_interface_id : String.t option
  ; owner_id : String.t option
  ; private_ip_addresses : InstanceSecondaryInterfacePrivateIpAddressList.t
  ; source_dest_check : Boolean.t option
  ; status : SecondaryInterfaceStatus.t option
  ; secondary_subnet_id : String.t option
  ; secondary_network_id : String.t option
  ; interface_type : SecondaryInterfaceType.t option
  }

let make
    ?attachment
    ?mac_address
    ?secondary_interface_id
    ?owner_id
    ?(private_ip_addresses = [])
    ?source_dest_check
    ?status
    ?secondary_subnet_id
    ?secondary_network_id
    ?interface_type
    () =
  { attachment
  ; mac_address
  ; secondary_interface_id
  ; owner_id
  ; private_ip_addresses
  ; source_dest_check
  ; status
  ; secondary_subnet_id
  ; secondary_network_id
  ; interface_type
  }

let parse xml =
  Some
    { attachment =
        Aws.Util.option_bind
          (Aws.Xml.member "attachment" xml)
          InstanceSecondaryInterfaceAttachment.parse
    ; mac_address = Aws.Util.option_bind (Aws.Xml.member "macAddress" xml) String.parse
    ; secondary_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "secondaryInterfaceId" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; private_ip_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "privateIpAddressSet" xml)
             InstanceSecondaryInterfacePrivateIpAddressList.parse)
    ; source_dest_check =
        Aws.Util.option_bind (Aws.Xml.member "sourceDestCheck" xml) Boolean.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "status" xml) SecondaryInterfaceStatus.parse
    ; secondary_subnet_id =
        Aws.Util.option_bind (Aws.Xml.member "secondarySubnetId" xml) String.parse
    ; secondary_network_id =
        Aws.Util.option_bind (Aws.Xml.member "secondaryNetworkId" xml) String.parse
    ; interface_type =
        Aws.Util.option_bind
          (Aws.Xml.member "interfaceType" xml)
          SecondaryInterfaceType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.interface_type (fun f ->
             Aws.Query.Pair ("InterfaceType", SecondaryInterfaceType.to_query f))
       ; Aws.Util.option_map v.secondary_network_id (fun f ->
             Aws.Query.Pair ("SecondaryNetworkId", String.to_query f))
       ; Aws.Util.option_map v.secondary_subnet_id (fun f ->
             Aws.Query.Pair ("SecondarySubnetId", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", SecondaryInterfaceStatus.to_query f))
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             Aws.Query.Pair ("SourceDestCheck", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PrivateIpAddressSet"
              , InstanceSecondaryInterfacePrivateIpAddressList.to_query
                  v.private_ip_addresses ))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.secondary_interface_id (fun f ->
             Aws.Query.Pair ("SecondaryInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.mac_address (fun f ->
             Aws.Query.Pair ("MacAddress", String.to_query f))
       ; Aws.Util.option_map v.attachment (fun f ->
             Aws.Query.Pair ("Attachment", InstanceSecondaryInterfaceAttachment.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.interface_type (fun f ->
             "interfaceType", SecondaryInterfaceType.to_json f)
       ; Aws.Util.option_map v.secondary_network_id (fun f ->
             "secondaryNetworkId", String.to_json f)
       ; Aws.Util.option_map v.secondary_subnet_id (fun f ->
             "secondarySubnetId", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "status", SecondaryInterfaceStatus.to_json f)
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             "sourceDestCheck", Boolean.to_json f)
       ; Some
           ( "privateIpAddressSet"
           , InstanceSecondaryInterfacePrivateIpAddressList.to_json v.private_ip_addresses
           )
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.secondary_interface_id (fun f ->
             "secondaryInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.mac_address (fun f -> "macAddress", String.to_json f)
       ; Aws.Util.option_map v.attachment (fun f ->
             "attachment", InstanceSecondaryInterfaceAttachment.to_json f)
       ])

let of_json j =
  { attachment =
      Aws.Util.option_map
        (Aws.Json.lookup j "attachment")
        InstanceSecondaryInterfaceAttachment.of_json
  ; mac_address = Aws.Util.option_map (Aws.Json.lookup j "macAddress") String.of_json
  ; secondary_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "secondaryInterfaceId") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; private_ip_addresses =
      InstanceSecondaryInterfacePrivateIpAddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "privateIpAddressSet"))
  ; source_dest_check =
      Aws.Util.option_map (Aws.Json.lookup j "sourceDestCheck") Boolean.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "status") SecondaryInterfaceStatus.of_json
  ; secondary_subnet_id =
      Aws.Util.option_map (Aws.Json.lookup j "secondarySubnetId") String.of_json
  ; secondary_network_id =
      Aws.Util.option_map (Aws.Json.lookup j "secondaryNetworkId") String.of_json
  ; interface_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "interfaceType")
        SecondaryInterfaceType.of_json
  }
