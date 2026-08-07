open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t option
  ; description : String.t option
  ; status : ClientVpnEndpointStatus.t option
  ; creation_time : String.t option
  ; deletion_time : String.t option
  ; dns_name : String.t option
  ; client_cidr_block : String.t option
  ; dns_servers : ValueStringList.t
  ; split_tunnel : Boolean.t option
  ; vpn_protocol : VpnProtocol.t option
  ; transport_protocol : TransportProtocol.t option
  ; vpn_port : Integer.t option
  ; associated_target_networks : AssociatedTargetNetworkSet.t
  ; server_certificate_arn : String.t option
  ; authentication_options : ClientVpnAuthenticationList.t
  ; connection_log_options : ConnectionLogResponseOptions.t option
  ; tags : TagList.t
  ; security_group_ids : ClientVpnSecurityGroupIdSet.t
  ; vpc_id : String.t option
  ; self_service_portal_url : String.t option
  ; client_connect_options : ClientConnectResponseOptions.t option
  ; session_timeout_hours : Integer.t option
  ; client_login_banner_options : ClientLoginBannerResponseOptions.t option
  ; client_route_enforcement_options : ClientRouteEnforcementResponseOptions.t option
  ; disconnect_on_session_timeout : Boolean.t option
  ; endpoint_ip_address_type : EndpointIpAddressType.t option
  ; traffic_ip_address_type : TrafficIpAddressType.t option
  ; transit_gateway_configuration :
      TransitGatewayConfigurationDescribeEndpointStructure.t option
  }

let make
    ?client_vpn_endpoint_id
    ?description
    ?status
    ?creation_time
    ?deletion_time
    ?dns_name
    ?client_cidr_block
    ?(dns_servers = [])
    ?split_tunnel
    ?vpn_protocol
    ?transport_protocol
    ?vpn_port
    ?(associated_target_networks = [])
    ?server_certificate_arn
    ?(authentication_options = [])
    ?connection_log_options
    ?(tags = [])
    ?(security_group_ids = [])
    ?vpc_id
    ?self_service_portal_url
    ?client_connect_options
    ?session_timeout_hours
    ?client_login_banner_options
    ?client_route_enforcement_options
    ?disconnect_on_session_timeout
    ?endpoint_ip_address_type
    ?traffic_ip_address_type
    ?transit_gateway_configuration
    () =
  { client_vpn_endpoint_id
  ; description
  ; status
  ; creation_time
  ; deletion_time
  ; dns_name
  ; client_cidr_block
  ; dns_servers
  ; split_tunnel
  ; vpn_protocol
  ; transport_protocol
  ; vpn_port
  ; associated_target_networks
  ; server_certificate_arn
  ; authentication_options
  ; connection_log_options
  ; tags
  ; security_group_ids
  ; vpc_id
  ; self_service_portal_url
  ; client_connect_options
  ; session_timeout_hours
  ; client_login_banner_options
  ; client_route_enforcement_options
  ; disconnect_on_session_timeout
  ; endpoint_ip_address_type
  ; traffic_ip_address_type
  ; transit_gateway_configuration
  }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "clientVpnEndpointId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "status" xml) ClientVpnEndpointStatus.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "creationTime" xml) String.parse
    ; deletion_time =
        Aws.Util.option_bind (Aws.Xml.member "deletionTime" xml) String.parse
    ; dns_name = Aws.Util.option_bind (Aws.Xml.member "dnsName" xml) String.parse
    ; client_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "clientCidrBlock" xml) String.parse
    ; dns_servers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "dnsServer" xml) ValueStringList.parse)
    ; split_tunnel = Aws.Util.option_bind (Aws.Xml.member "splitTunnel" xml) Boolean.parse
    ; vpn_protocol =
        Aws.Util.option_bind (Aws.Xml.member "vpnProtocol" xml) VpnProtocol.parse
    ; transport_protocol =
        Aws.Util.option_bind
          (Aws.Xml.member "transportProtocol" xml)
          TransportProtocol.parse
    ; vpn_port = Aws.Util.option_bind (Aws.Xml.member "vpnPort" xml) Integer.parse
    ; associated_target_networks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "associatedTargetNetwork" xml)
             AssociatedTargetNetworkSet.parse)
    ; server_certificate_arn =
        Aws.Util.option_bind (Aws.Xml.member "serverCertificateArn" xml) String.parse
    ; authentication_options =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "authenticationOptions" xml)
             ClientVpnAuthenticationList.parse)
    ; connection_log_options =
        Aws.Util.option_bind
          (Aws.Xml.member "connectionLogOptions" xml)
          ConnectionLogResponseOptions.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "securityGroupIdSet" xml)
             ClientVpnSecurityGroupIdSet.parse)
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; self_service_portal_url =
        Aws.Util.option_bind (Aws.Xml.member "selfServicePortalUrl" xml) String.parse
    ; client_connect_options =
        Aws.Util.option_bind
          (Aws.Xml.member "clientConnectOptions" xml)
          ClientConnectResponseOptions.parse
    ; session_timeout_hours =
        Aws.Util.option_bind (Aws.Xml.member "sessionTimeoutHours" xml) Integer.parse
    ; client_login_banner_options =
        Aws.Util.option_bind
          (Aws.Xml.member "clientLoginBannerOptions" xml)
          ClientLoginBannerResponseOptions.parse
    ; client_route_enforcement_options =
        Aws.Util.option_bind
          (Aws.Xml.member "clientRouteEnforcementOptions" xml)
          ClientRouteEnforcementResponseOptions.parse
    ; disconnect_on_session_timeout =
        Aws.Util.option_bind
          (Aws.Xml.member "disconnectOnSessionTimeout" xml)
          Boolean.parse
    ; endpoint_ip_address_type =
        Aws.Util.option_bind
          (Aws.Xml.member "endpointIpAddressType" xml)
          EndpointIpAddressType.parse
    ; traffic_ip_address_type =
        Aws.Util.option_bind
          (Aws.Xml.member "trafficIpAddressType" xml)
          TrafficIpAddressType.parse
    ; transit_gateway_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayConfiguration" xml)
          TransitGatewayConfigurationDescribeEndpointStructure.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_configuration (fun f ->
             Aws.Query.Pair
               ( "TransitGatewayConfiguration"
               , TransitGatewayConfigurationDescribeEndpointStructure.to_query f ))
       ; Aws.Util.option_map v.traffic_ip_address_type (fun f ->
             Aws.Query.Pair ("TrafficIpAddressType", TrafficIpAddressType.to_query f))
       ; Aws.Util.option_map v.endpoint_ip_address_type (fun f ->
             Aws.Query.Pair ("EndpointIpAddressType", EndpointIpAddressType.to_query f))
       ; Aws.Util.option_map v.disconnect_on_session_timeout (fun f ->
             Aws.Query.Pair ("DisconnectOnSessionTimeout", Boolean.to_query f))
       ; Aws.Util.option_map v.client_route_enforcement_options (fun f ->
             Aws.Query.Pair
               ( "ClientRouteEnforcementOptions"
               , ClientRouteEnforcementResponseOptions.to_query f ))
       ; Aws.Util.option_map v.client_login_banner_options (fun f ->
             Aws.Query.Pair
               ("ClientLoginBannerOptions", ClientLoginBannerResponseOptions.to_query f))
       ; Aws.Util.option_map v.session_timeout_hours (fun f ->
             Aws.Query.Pair ("SessionTimeoutHours", Integer.to_query f))
       ; Aws.Util.option_map v.client_connect_options (fun f ->
             Aws.Query.Pair
               ("ClientConnectOptions", ClientConnectResponseOptions.to_query f))
       ; Aws.Util.option_map v.self_service_portal_url (fun f ->
             Aws.Query.Pair ("SelfServicePortalUrl", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupIdSet"
              , ClientVpnSecurityGroupIdSet.to_query v.security_group_ids ))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.connection_log_options (fun f ->
             Aws.Query.Pair
               ("ConnectionLogOptions", ConnectionLogResponseOptions.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AuthenticationOptions"
              , ClientVpnAuthenticationList.to_query v.authentication_options ))
       ; Aws.Util.option_map v.server_certificate_arn (fun f ->
             Aws.Query.Pair ("ServerCertificateArn", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AssociatedTargetNetwork"
              , AssociatedTargetNetworkSet.to_query v.associated_target_networks ))
       ; Aws.Util.option_map v.vpn_port (fun f ->
             Aws.Query.Pair ("VpnPort", Integer.to_query f))
       ; Aws.Util.option_map v.transport_protocol (fun f ->
             Aws.Query.Pair ("TransportProtocol", TransportProtocol.to_query f))
       ; Aws.Util.option_map v.vpn_protocol (fun f ->
             Aws.Query.Pair ("VpnProtocol", VpnProtocol.to_query f))
       ; Aws.Util.option_map v.split_tunnel (fun f ->
             Aws.Query.Pair ("SplitTunnel", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("DnsServer", ValueStringList.to_query v.dns_servers))
       ; Aws.Util.option_map v.client_cidr_block (fun f ->
             Aws.Query.Pair ("ClientCidrBlock", String.to_query f))
       ; Aws.Util.option_map v.dns_name (fun f ->
             Aws.Query.Pair ("DnsName", String.to_query f))
       ; Aws.Util.option_map v.deletion_time (fun f ->
             Aws.Query.Pair ("DeletionTime", String.to_query f))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ClientVpnEndpointStatus.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             Aws.Query.Pair ("ClientVpnEndpointId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_configuration (fun f ->
             ( "transitGatewayConfiguration"
             , TransitGatewayConfigurationDescribeEndpointStructure.to_json f ))
       ; Aws.Util.option_map v.traffic_ip_address_type (fun f ->
             "trafficIpAddressType", TrafficIpAddressType.to_json f)
       ; Aws.Util.option_map v.endpoint_ip_address_type (fun f ->
             "endpointIpAddressType", EndpointIpAddressType.to_json f)
       ; Aws.Util.option_map v.disconnect_on_session_timeout (fun f ->
             "disconnectOnSessionTimeout", Boolean.to_json f)
       ; Aws.Util.option_map v.client_route_enforcement_options (fun f ->
             ( "clientRouteEnforcementOptions"
             , ClientRouteEnforcementResponseOptions.to_json f ))
       ; Aws.Util.option_map v.client_login_banner_options (fun f ->
             "clientLoginBannerOptions", ClientLoginBannerResponseOptions.to_json f)
       ; Aws.Util.option_map v.session_timeout_hours (fun f ->
             "sessionTimeoutHours", Integer.to_json f)
       ; Aws.Util.option_map v.client_connect_options (fun f ->
             "clientConnectOptions", ClientConnectResponseOptions.to_json f)
       ; Aws.Util.option_map v.self_service_portal_url (fun f ->
             "selfServicePortalUrl", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Some
           ("securityGroupIdSet", ClientVpnSecurityGroupIdSet.to_json v.security_group_ids)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.connection_log_options (fun f ->
             "connectionLogOptions", ConnectionLogResponseOptions.to_json f)
       ; Some
           ( "authenticationOptions"
           , ClientVpnAuthenticationList.to_json v.authentication_options )
       ; Aws.Util.option_map v.server_certificate_arn (fun f ->
             "serverCertificateArn", String.to_json f)
       ; Some
           ( "associatedTargetNetwork"
           , AssociatedTargetNetworkSet.to_json v.associated_target_networks )
       ; Aws.Util.option_map v.vpn_port (fun f -> "vpnPort", Integer.to_json f)
       ; Aws.Util.option_map v.transport_protocol (fun f ->
             "transportProtocol", TransportProtocol.to_json f)
       ; Aws.Util.option_map v.vpn_protocol (fun f ->
             "vpnProtocol", VpnProtocol.to_json f)
       ; Aws.Util.option_map v.split_tunnel (fun f -> "splitTunnel", Boolean.to_json f)
       ; Some ("dnsServer", ValueStringList.to_json v.dns_servers)
       ; Aws.Util.option_map v.client_cidr_block (fun f ->
             "clientCidrBlock", String.to_json f)
       ; Aws.Util.option_map v.dns_name (fun f -> "dnsName", String.to_json f)
       ; Aws.Util.option_map v.deletion_time (fun f -> "deletionTime", String.to_json f)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "status", ClientVpnEndpointStatus.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             "clientVpnEndpointId", String.to_json f)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "clientVpnEndpointId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "status") ClientVpnEndpointStatus.of_json
  ; creation_time = Aws.Util.option_map (Aws.Json.lookup j "creationTime") String.of_json
  ; deletion_time = Aws.Util.option_map (Aws.Json.lookup j "deletionTime") String.of_json
  ; dns_name = Aws.Util.option_map (Aws.Json.lookup j "dnsName") String.of_json
  ; client_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "clientCidrBlock") String.of_json
  ; dns_servers =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "dnsServer"))
  ; split_tunnel = Aws.Util.option_map (Aws.Json.lookup j "splitTunnel") Boolean.of_json
  ; vpn_protocol =
      Aws.Util.option_map (Aws.Json.lookup j "vpnProtocol") VpnProtocol.of_json
  ; transport_protocol =
      Aws.Util.option_map
        (Aws.Json.lookup j "transportProtocol")
        TransportProtocol.of_json
  ; vpn_port = Aws.Util.option_map (Aws.Json.lookup j "vpnPort") Integer.of_json
  ; associated_target_networks =
      AssociatedTargetNetworkSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "associatedTargetNetwork"))
  ; server_certificate_arn =
      Aws.Util.option_map (Aws.Json.lookup j "serverCertificateArn") String.of_json
  ; authentication_options =
      ClientVpnAuthenticationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "authenticationOptions"))
  ; connection_log_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "connectionLogOptions")
        ConnectionLogResponseOptions.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; security_group_ids =
      ClientVpnSecurityGroupIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "securityGroupIdSet"))
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; self_service_portal_url =
      Aws.Util.option_map (Aws.Json.lookup j "selfServicePortalUrl") String.of_json
  ; client_connect_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "clientConnectOptions")
        ClientConnectResponseOptions.of_json
  ; session_timeout_hours =
      Aws.Util.option_map (Aws.Json.lookup j "sessionTimeoutHours") Integer.of_json
  ; client_login_banner_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "clientLoginBannerOptions")
        ClientLoginBannerResponseOptions.of_json
  ; client_route_enforcement_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "clientRouteEnforcementOptions")
        ClientRouteEnforcementResponseOptions.of_json
  ; disconnect_on_session_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "disconnectOnSessionTimeout") Boolean.of_json
  ; endpoint_ip_address_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "endpointIpAddressType")
        EndpointIpAddressType.of_json
  ; traffic_ip_address_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "trafficIpAddressType")
        TrafficIpAddressType.of_json
  ; transit_gateway_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayConfiguration")
        TransitGatewayConfigurationDescribeEndpointStructure.of_json
  }
