open Aws.BaseTypes

type t =
  { availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; attachment : SecondaryInterfaceAttachment.t option
  ; mac_address : String.t option
  ; owner_id : String.t option
  ; private_ipv4_addresses : SecondaryInterfaceIpv4AddressList.t
  ; secondary_interface_id : String.t option
  ; secondary_interface_arn : String.t option
  ; secondary_interface_type : SecondaryInterfaceType.t option
  ; secondary_subnet_id : String.t option
  ; secondary_network_id : String.t option
  ; secondary_network_type : SecondaryNetworkType.t option
  ; source_dest_check : Boolean.t option
  ; status : SecondaryInterfaceStatus.t option
  ; tags : TagList.t
  }

let make
    ?availability_zone
    ?availability_zone_id
    ?attachment
    ?mac_address
    ?owner_id
    ?(private_ipv4_addresses = [])
    ?secondary_interface_id
    ?secondary_interface_arn
    ?secondary_interface_type
    ?secondary_subnet_id
    ?secondary_network_id
    ?secondary_network_type
    ?source_dest_check
    ?status
    ?(tags = [])
    () =
  { availability_zone
  ; availability_zone_id
  ; attachment
  ; mac_address
  ; owner_id
  ; private_ipv4_addresses
  ; secondary_interface_id
  ; secondary_interface_arn
  ; secondary_interface_type
  ; secondary_subnet_id
  ; secondary_network_id
  ; secondary_network_type
  ; source_dest_check
  ; status
  ; tags
  }

let parse xml =
  Some
    { availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; attachment =
        Aws.Util.option_bind
          (Aws.Xml.member "attachment" xml)
          SecondaryInterfaceAttachment.parse
    ; mac_address = Aws.Util.option_bind (Aws.Xml.member "macAddress" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; private_ipv4_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "privateIpv4AddressSet" xml)
             SecondaryInterfaceIpv4AddressList.parse)
    ; secondary_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "secondaryInterfaceId" xml) String.parse
    ; secondary_interface_arn =
        Aws.Util.option_bind (Aws.Xml.member "secondaryInterfaceArn" xml) String.parse
    ; secondary_interface_type =
        Aws.Util.option_bind
          (Aws.Xml.member "secondaryInterfaceType" xml)
          SecondaryInterfaceType.parse
    ; secondary_subnet_id =
        Aws.Util.option_bind (Aws.Xml.member "secondarySubnetId" xml) String.parse
    ; secondary_network_id =
        Aws.Util.option_bind (Aws.Xml.member "secondaryNetworkId" xml) String.parse
    ; secondary_network_type =
        Aws.Util.option_bind
          (Aws.Xml.member "secondaryNetworkType" xml)
          SecondaryNetworkType.parse
    ; source_dest_check =
        Aws.Util.option_bind (Aws.Xml.member "sourceDestCheck" xml) Boolean.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "status" xml) SecondaryInterfaceStatus.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", SecondaryInterfaceStatus.to_query f))
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             Aws.Query.Pair ("SourceDestCheck", Boolean.to_query f))
       ; Aws.Util.option_map v.secondary_network_type (fun f ->
             Aws.Query.Pair ("SecondaryNetworkType", SecondaryNetworkType.to_query f))
       ; Aws.Util.option_map v.secondary_network_id (fun f ->
             Aws.Query.Pair ("SecondaryNetworkId", String.to_query f))
       ; Aws.Util.option_map v.secondary_subnet_id (fun f ->
             Aws.Query.Pair ("SecondarySubnetId", String.to_query f))
       ; Aws.Util.option_map v.secondary_interface_type (fun f ->
             Aws.Query.Pair ("SecondaryInterfaceType", SecondaryInterfaceType.to_query f))
       ; Aws.Util.option_map v.secondary_interface_arn (fun f ->
             Aws.Query.Pair ("SecondaryInterfaceArn", String.to_query f))
       ; Aws.Util.option_map v.secondary_interface_id (fun f ->
             Aws.Query.Pair ("SecondaryInterfaceId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PrivateIpv4AddressSet"
              , SecondaryInterfaceIpv4AddressList.to_query v.private_ipv4_addresses ))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.mac_address (fun f ->
             Aws.Query.Pair ("MacAddress", String.to_query f))
       ; Aws.Util.option_map v.attachment (fun f ->
             Aws.Query.Pair ("Attachment", SecondaryInterfaceAttachment.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.status (fun f ->
             "status", SecondaryInterfaceStatus.to_json f)
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             "sourceDestCheck", Boolean.to_json f)
       ; Aws.Util.option_map v.secondary_network_type (fun f ->
             "secondaryNetworkType", SecondaryNetworkType.to_json f)
       ; Aws.Util.option_map v.secondary_network_id (fun f ->
             "secondaryNetworkId", String.to_json f)
       ; Aws.Util.option_map v.secondary_subnet_id (fun f ->
             "secondarySubnetId", String.to_json f)
       ; Aws.Util.option_map v.secondary_interface_type (fun f ->
             "secondaryInterfaceType", SecondaryInterfaceType.to_json f)
       ; Aws.Util.option_map v.secondary_interface_arn (fun f ->
             "secondaryInterfaceArn", String.to_json f)
       ; Aws.Util.option_map v.secondary_interface_id (fun f ->
             "secondaryInterfaceId", String.to_json f)
       ; Some
           ( "privateIpv4AddressSet"
           , SecondaryInterfaceIpv4AddressList.to_json v.private_ipv4_addresses )
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.mac_address (fun f -> "macAddress", String.to_json f)
       ; Aws.Util.option_map v.attachment (fun f ->
             "attachment", SecondaryInterfaceAttachment.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ])

let of_json j =
  { availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; attachment =
      Aws.Util.option_map
        (Aws.Json.lookup j "attachment")
        SecondaryInterfaceAttachment.of_json
  ; mac_address = Aws.Util.option_map (Aws.Json.lookup j "macAddress") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; private_ipv4_addresses =
      SecondaryInterfaceIpv4AddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "privateIpv4AddressSet"))
  ; secondary_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "secondaryInterfaceId") String.of_json
  ; secondary_interface_arn =
      Aws.Util.option_map (Aws.Json.lookup j "secondaryInterfaceArn") String.of_json
  ; secondary_interface_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "secondaryInterfaceType")
        SecondaryInterfaceType.of_json
  ; secondary_subnet_id =
      Aws.Util.option_map (Aws.Json.lookup j "secondarySubnetId") String.of_json
  ; secondary_network_id =
      Aws.Util.option_map (Aws.Json.lookup j "secondaryNetworkId") String.of_json
  ; secondary_network_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "secondaryNetworkType")
        SecondaryNetworkType.of_json
  ; source_dest_check =
      Aws.Util.option_map (Aws.Json.lookup j "sourceDestCheck") Boolean.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "status") SecondaryInterfaceStatus.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
