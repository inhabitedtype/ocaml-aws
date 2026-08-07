open Aws.BaseTypes

type t =
  { owner_id : String.t option
  ; instance_connect_endpoint_id : String.t option
  ; instance_connect_endpoint_arn : String.t option
  ; state : Ec2InstanceConnectEndpointState.t option
  ; state_message : String.t option
  ; dns_name : String.t option
  ; fips_dns_name : String.t option
  ; network_interface_ids : NetworkInterfaceIdSet.t
  ; vpc_id : String.t option
  ; availability_zone : String.t option
  ; created_at : DateTime.t option
  ; subnet_id : String.t option
  ; preserve_client_ip : Boolean.t option
  ; security_group_ids : SecurityGroupIdSet.t
  ; tags : TagList.t
  ; ip_address_type : IpAddressType.t option
  ; public_dns_names : InstanceConnectEndpointPublicDnsNames.t option
  ; availability_zone_id : String.t option
  }

let make
    ?owner_id
    ?instance_connect_endpoint_id
    ?instance_connect_endpoint_arn
    ?state
    ?state_message
    ?dns_name
    ?fips_dns_name
    ?(network_interface_ids = [])
    ?vpc_id
    ?availability_zone
    ?created_at
    ?subnet_id
    ?preserve_client_ip
    ?(security_group_ids = [])
    ?(tags = [])
    ?ip_address_type
    ?public_dns_names
    ?availability_zone_id
    () =
  { owner_id
  ; instance_connect_endpoint_id
  ; instance_connect_endpoint_arn
  ; state
  ; state_message
  ; dns_name
  ; fips_dns_name
  ; network_interface_ids
  ; vpc_id
  ; availability_zone
  ; created_at
  ; subnet_id
  ; preserve_client_ip
  ; security_group_ids
  ; tags
  ; ip_address_type
  ; public_dns_names
  ; availability_zone_id
  }

let parse xml =
  Some
    { owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; instance_connect_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "instanceConnectEndpointId" xml) String.parse
    ; instance_connect_endpoint_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceConnectEndpointArn" xml)
          String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          Ec2InstanceConnectEndpointState.parse
    ; state_message =
        Aws.Util.option_bind (Aws.Xml.member "stateMessage" xml) String.parse
    ; dns_name = Aws.Util.option_bind (Aws.Xml.member "dnsName" xml) String.parse
    ; fips_dns_name = Aws.Util.option_bind (Aws.Xml.member "fipsDnsName" xml) String.parse
    ; network_interface_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkInterfaceIdSet" xml)
             NetworkInterfaceIdSet.parse)
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; created_at = Aws.Util.option_bind (Aws.Xml.member "createdAt" xml) DateTime.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; preserve_client_ip =
        Aws.Util.option_bind (Aws.Xml.member "preserveClientIp" xml) Boolean.parse
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "securityGroupIdSet" xml)
             SecurityGroupIdSet.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; ip_address_type =
        Aws.Util.option_bind (Aws.Xml.member "ipAddressType" xml) IpAddressType.parse
    ; public_dns_names =
        Aws.Util.option_bind
          (Aws.Xml.member "publicDnsNames" xml)
          InstanceConnectEndpointPublicDnsNames.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.public_dns_names (fun f ->
             Aws.Query.Pair
               ("PublicDnsNames", InstanceConnectEndpointPublicDnsNames.to_query f))
       ; Aws.Util.option_map v.ip_address_type (fun f ->
             Aws.Query.Pair ("IpAddressType", IpAddressType.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ("SecurityGroupIdSet", SecurityGroupIdSet.to_query v.security_group_ids))
       ; Aws.Util.option_map v.preserve_client_ip (fun f ->
             Aws.Query.Pair ("PreserveClientIp", Boolean.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.created_at (fun f ->
             Aws.Query.Pair ("CreatedAt", DateTime.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInterfaceIdSet"
              , NetworkInterfaceIdSet.to_query v.network_interface_ids ))
       ; Aws.Util.option_map v.fips_dns_name (fun f ->
             Aws.Query.Pair ("FipsDnsName", String.to_query f))
       ; Aws.Util.option_map v.dns_name (fun f ->
             Aws.Query.Pair ("DnsName", String.to_query f))
       ; Aws.Util.option_map v.state_message (fun f ->
             Aws.Query.Pair ("StateMessage", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", Ec2InstanceConnectEndpointState.to_query f))
       ; Aws.Util.option_map v.instance_connect_endpoint_arn (fun f ->
             Aws.Query.Pair ("InstanceConnectEndpointArn", String.to_query f))
       ; Aws.Util.option_map v.instance_connect_endpoint_id (fun f ->
             Aws.Query.Pair ("InstanceConnectEndpointId", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.public_dns_names (fun f ->
             "publicDnsNames", InstanceConnectEndpointPublicDnsNames.to_json f)
       ; Aws.Util.option_map v.ip_address_type (fun f ->
             "ipAddressType", IpAddressType.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Some ("securityGroupIdSet", SecurityGroupIdSet.to_json v.security_group_ids)
       ; Aws.Util.option_map v.preserve_client_ip (fun f ->
             "preserveClientIp", Boolean.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.created_at (fun f -> "createdAt", DateTime.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Some
           ("networkInterfaceIdSet", NetworkInterfaceIdSet.to_json v.network_interface_ids)
       ; Aws.Util.option_map v.fips_dns_name (fun f -> "fipsDnsName", String.to_json f)
       ; Aws.Util.option_map v.dns_name (fun f -> "dnsName", String.to_json f)
       ; Aws.Util.option_map v.state_message (fun f -> "stateMessage", String.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", Ec2InstanceConnectEndpointState.to_json f)
       ; Aws.Util.option_map v.instance_connect_endpoint_arn (fun f ->
             "instanceConnectEndpointArn", String.to_json f)
       ; Aws.Util.option_map v.instance_connect_endpoint_id (fun f ->
             "instanceConnectEndpointId", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ])

let of_json j =
  { owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; instance_connect_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "instanceConnectEndpointId") String.of_json
  ; instance_connect_endpoint_arn =
      Aws.Util.option_map (Aws.Json.lookup j "instanceConnectEndpointArn") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        Ec2InstanceConnectEndpointState.of_json
  ; state_message = Aws.Util.option_map (Aws.Json.lookup j "stateMessage") String.of_json
  ; dns_name = Aws.Util.option_map (Aws.Json.lookup j "dnsName") String.of_json
  ; fips_dns_name = Aws.Util.option_map (Aws.Json.lookup j "fipsDnsName") String.of_json
  ; network_interface_ids =
      NetworkInterfaceIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterfaceIdSet"))
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; created_at = Aws.Util.option_map (Aws.Json.lookup j "createdAt") DateTime.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; preserve_client_ip =
      Aws.Util.option_map (Aws.Json.lookup j "preserveClientIp") Boolean.of_json
  ; security_group_ids =
      SecurityGroupIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "securityGroupIdSet"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; ip_address_type =
      Aws.Util.option_map (Aws.Json.lookup j "ipAddressType") IpAddressType.of_json
  ; public_dns_names =
      Aws.Util.option_map
        (Aws.Json.lookup j "publicDnsNames")
        InstanceConnectEndpointPublicDnsNames.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  }
