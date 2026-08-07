open Aws.BaseTypes

type t =
  { client_cidr_block : String.t option
  ; server_certificate_arn : String.t
  ; authentication_options : ClientVpnAuthenticationRequestList.t
  ; connection_log_options : ConnectionLogOptions.t
  ; dns_servers : ValueStringList.t
  ; transport_protocol : TransportProtocol.t option
  ; vpn_port : Integer.t option
  ; description : String.t option
  ; split_tunnel : Boolean.t option
  ; dry_run : Boolean.t option
  ; client_token : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; security_group_ids : ClientVpnSecurityGroupIdSet.t
  ; vpc_id : String.t option
  ; self_service_portal : SelfServicePortal.t option
  ; client_connect_options : ClientConnectOptions.t option
  ; session_timeout_hours : Integer.t option
  ; client_login_banner_options : ClientLoginBannerOptions.t option
  ; client_route_enforcement_options : ClientRouteEnforcementOptions.t option
  ; disconnect_on_session_timeout : Boolean.t option
  ; endpoint_ip_address_type : EndpointIpAddressType.t option
  ; traffic_ip_address_type : TrafficIpAddressType.t option
  ; transit_gateway_configuration : TransitGatewayConfigurationInputStructure.t option
  }

let make
    ?client_cidr_block
    ~server_certificate_arn
    ~authentication_options
    ~connection_log_options
    ?(dns_servers = [])
    ?transport_protocol
    ?vpn_port
    ?description
    ?split_tunnel
    ?dry_run
    ?client_token
    ?(tag_specifications = [])
    ?(security_group_ids = [])
    ?vpc_id
    ?self_service_portal
    ?client_connect_options
    ?session_timeout_hours
    ?client_login_banner_options
    ?client_route_enforcement_options
    ?disconnect_on_session_timeout
    ?endpoint_ip_address_type
    ?traffic_ip_address_type
    ?transit_gateway_configuration
    () =
  { client_cidr_block
  ; server_certificate_arn
  ; authentication_options
  ; connection_log_options
  ; dns_servers
  ; transport_protocol
  ; vpn_port
  ; description
  ; split_tunnel
  ; dry_run
  ; client_token
  ; tag_specifications
  ; security_group_ids
  ; vpc_id
  ; self_service_portal
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
    { client_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "ClientCidrBlock" xml) String.parse
    ; server_certificate_arn =
        Aws.Xml.required
          "ServerCertificateArn"
          (Aws.Util.option_bind (Aws.Xml.member "ServerCertificateArn" xml) String.parse)
    ; authentication_options =
        Aws.Xml.required
          "Authentication"
          (Aws.Util.option_bind
             (Aws.Xml.member "Authentication" xml)
             ClientVpnAuthenticationRequestList.parse)
    ; connection_log_options =
        Aws.Xml.required
          "ConnectionLogOptions"
          (Aws.Util.option_bind
             (Aws.Xml.member "ConnectionLogOptions" xml)
             ConnectionLogOptions.parse)
    ; dns_servers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "DnsServers" xml) ValueStringList.parse)
    ; transport_protocol =
        Aws.Util.option_bind
          (Aws.Xml.member "TransportProtocol" xml)
          TransportProtocol.parse
    ; vpn_port = Aws.Util.option_bind (Aws.Xml.member "VpnPort" xml) Integer.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; split_tunnel = Aws.Util.option_bind (Aws.Xml.member "SplitTunnel" xml) Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupId" xml)
             ClientVpnSecurityGroupIdSet.parse)
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
    ; self_service_portal =
        Aws.Util.option_bind
          (Aws.Xml.member "SelfServicePortal" xml)
          SelfServicePortal.parse
    ; client_connect_options =
        Aws.Util.option_bind
          (Aws.Xml.member "ClientConnectOptions" xml)
          ClientConnectOptions.parse
    ; session_timeout_hours =
        Aws.Util.option_bind (Aws.Xml.member "SessionTimeoutHours" xml) Integer.parse
    ; client_login_banner_options =
        Aws.Util.option_bind
          (Aws.Xml.member "ClientLoginBannerOptions" xml)
          ClientLoginBannerOptions.parse
    ; client_route_enforcement_options =
        Aws.Util.option_bind
          (Aws.Xml.member "ClientRouteEnforcementOptions" xml)
          ClientRouteEnforcementOptions.parse
    ; disconnect_on_session_timeout =
        Aws.Util.option_bind
          (Aws.Xml.member "DisconnectOnSessionTimeout" xml)
          Boolean.parse
    ; endpoint_ip_address_type =
        Aws.Util.option_bind
          (Aws.Xml.member "EndpointIpAddressType" xml)
          EndpointIpAddressType.parse
    ; traffic_ip_address_type =
        Aws.Util.option_bind
          (Aws.Xml.member "TrafficIpAddressType" xml)
          TrafficIpAddressType.parse
    ; transit_gateway_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "TransitGatewayConfiguration" xml)
          TransitGatewayConfigurationInputStructure.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_configuration (fun f ->
             Aws.Query.Pair
               ( "TransitGatewayConfiguration"
               , TransitGatewayConfigurationInputStructure.to_query f ))
       ; Aws.Util.option_map v.traffic_ip_address_type (fun f ->
             Aws.Query.Pair ("TrafficIpAddressType", TrafficIpAddressType.to_query f))
       ; Aws.Util.option_map v.endpoint_ip_address_type (fun f ->
             Aws.Query.Pair ("EndpointIpAddressType", EndpointIpAddressType.to_query f))
       ; Aws.Util.option_map v.disconnect_on_session_timeout (fun f ->
             Aws.Query.Pair ("DisconnectOnSessionTimeout", Boolean.to_query f))
       ; Aws.Util.option_map v.client_route_enforcement_options (fun f ->
             Aws.Query.Pair
               ("ClientRouteEnforcementOptions", ClientRouteEnforcementOptions.to_query f))
       ; Aws.Util.option_map v.client_login_banner_options (fun f ->
             Aws.Query.Pair
               ("ClientLoginBannerOptions", ClientLoginBannerOptions.to_query f))
       ; Aws.Util.option_map v.session_timeout_hours (fun f ->
             Aws.Query.Pair ("SessionTimeoutHours", Integer.to_query f))
       ; Aws.Util.option_map v.client_connect_options (fun f ->
             Aws.Query.Pair ("ClientConnectOptions", ClientConnectOptions.to_query f))
       ; Aws.Util.option_map v.self_service_portal (fun f ->
             Aws.Query.Pair ("SelfServicePortal", SelfServicePortal.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupId"
              , ClientVpnSecurityGroupIdSet.to_query v.security_group_ids ))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.split_tunnel (fun f ->
             Aws.Query.Pair ("SplitTunnel", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.vpn_port (fun f ->
             Aws.Query.Pair ("VpnPort", Integer.to_query f))
       ; Aws.Util.option_map v.transport_protocol (fun f ->
             Aws.Query.Pair ("TransportProtocol", TransportProtocol.to_query f))
       ; Some (Aws.Query.Pair ("DnsServers", ValueStringList.to_query v.dns_servers))
       ; Some
           (Aws.Query.Pair
              ( "ConnectionLogOptions"
              , ConnectionLogOptions.to_query v.connection_log_options ))
       ; Some
           (Aws.Query.Pair
              ( "Authentication"
              , ClientVpnAuthenticationRequestList.to_query v.authentication_options ))
       ; Some
           (Aws.Query.Pair
              ("ServerCertificateArn", String.to_query v.server_certificate_arn))
       ; Aws.Util.option_map v.client_cidr_block (fun f ->
             Aws.Query.Pair ("ClientCidrBlock", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_configuration (fun f ->
             ( "TransitGatewayConfiguration"
             , TransitGatewayConfigurationInputStructure.to_json f ))
       ; Aws.Util.option_map v.traffic_ip_address_type (fun f ->
             "TrafficIpAddressType", TrafficIpAddressType.to_json f)
       ; Aws.Util.option_map v.endpoint_ip_address_type (fun f ->
             "EndpointIpAddressType", EndpointIpAddressType.to_json f)
       ; Aws.Util.option_map v.disconnect_on_session_timeout (fun f ->
             "DisconnectOnSessionTimeout", Boolean.to_json f)
       ; Aws.Util.option_map v.client_route_enforcement_options (fun f ->
             "ClientRouteEnforcementOptions", ClientRouteEnforcementOptions.to_json f)
       ; Aws.Util.option_map v.client_login_banner_options (fun f ->
             "ClientLoginBannerOptions", ClientLoginBannerOptions.to_json f)
       ; Aws.Util.option_map v.session_timeout_hours (fun f ->
             "SessionTimeoutHours", Integer.to_json f)
       ; Aws.Util.option_map v.client_connect_options (fun f ->
             "ClientConnectOptions", ClientConnectOptions.to_json f)
       ; Aws.Util.option_map v.self_service_portal (fun f ->
             "SelfServicePortal", SelfServicePortal.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
       ; Some ("SecurityGroupId", ClientVpnSecurityGroupIdSet.to_json v.security_group_ids)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.split_tunnel (fun f -> "SplitTunnel", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.vpn_port (fun f -> "VpnPort", Integer.to_json f)
       ; Aws.Util.option_map v.transport_protocol (fun f ->
             "TransportProtocol", TransportProtocol.to_json f)
       ; Some ("DnsServers", ValueStringList.to_json v.dns_servers)
       ; Some
           ("ConnectionLogOptions", ConnectionLogOptions.to_json v.connection_log_options)
       ; Some
           ( "Authentication"
           , ClientVpnAuthenticationRequestList.to_json v.authentication_options )
       ; Some ("ServerCertificateArn", String.to_json v.server_certificate_arn)
       ; Aws.Util.option_map v.client_cidr_block (fun f ->
             "ClientCidrBlock", String.to_json f)
       ])

let of_json j =
  { client_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "ClientCidrBlock") String.of_json
  ; server_certificate_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ServerCertificateArn"))
  ; authentication_options =
      ClientVpnAuthenticationRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Authentication"))
  ; connection_log_options =
      ConnectionLogOptions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ConnectionLogOptions"))
  ; dns_servers =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DnsServers"))
  ; transport_protocol =
      Aws.Util.option_map
        (Aws.Json.lookup j "TransportProtocol")
        TransportProtocol.of_json
  ; vpn_port = Aws.Util.option_map (Aws.Json.lookup j "VpnPort") Integer.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; split_tunnel = Aws.Util.option_map (Aws.Json.lookup j "SplitTunnel") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; security_group_ids =
      ClientVpnSecurityGroupIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupId"))
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
  ; self_service_portal =
      Aws.Util.option_map
        (Aws.Json.lookup j "SelfServicePortal")
        SelfServicePortal.of_json
  ; client_connect_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "ClientConnectOptions")
        ClientConnectOptions.of_json
  ; session_timeout_hours =
      Aws.Util.option_map (Aws.Json.lookup j "SessionTimeoutHours") Integer.of_json
  ; client_login_banner_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "ClientLoginBannerOptions")
        ClientLoginBannerOptions.of_json
  ; client_route_enforcement_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "ClientRouteEnforcementOptions")
        ClientRouteEnforcementOptions.of_json
  ; disconnect_on_session_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "DisconnectOnSessionTimeout") Boolean.of_json
  ; endpoint_ip_address_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "EndpointIpAddressType")
        EndpointIpAddressType.of_json
  ; traffic_ip_address_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "TrafficIpAddressType")
        TrafficIpAddressType.of_json
  ; transit_gateway_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "TransitGatewayConfiguration")
        TransitGatewayConfigurationInputStructure.of_json
  }
