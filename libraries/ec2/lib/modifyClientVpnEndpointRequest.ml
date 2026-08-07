open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t
  ; server_certificate_arn : String.t option
  ; connection_log_options : ConnectionLogOptions.t option
  ; dns_servers : DnsServersOptionsModifyStructure.t option
  ; vpn_port : Integer.t option
  ; description : String.t option
  ; split_tunnel : Boolean.t option
  ; dry_run : Boolean.t option
  ; security_group_ids : ClientVpnSecurityGroupIdSet.t
  ; vpc_id : String.t option
  ; self_service_portal : SelfServicePortal.t option
  ; client_connect_options : ClientConnectOptions.t option
  ; session_timeout_hours : Integer.t option
  ; client_login_banner_options : ClientLoginBannerOptions.t option
  ; client_route_enforcement_options : ClientRouteEnforcementOptions.t option
  ; disconnect_on_session_timeout : Boolean.t option
  ; transit_gateway_configuration : TransitGatewayConfigurationInputStructure.t option
  }

let make
    ~client_vpn_endpoint_id
    ?server_certificate_arn
    ?connection_log_options
    ?dns_servers
    ?vpn_port
    ?description
    ?split_tunnel
    ?dry_run
    ?(security_group_ids = [])
    ?vpc_id
    ?self_service_portal
    ?client_connect_options
    ?session_timeout_hours
    ?client_login_banner_options
    ?client_route_enforcement_options
    ?disconnect_on_session_timeout
    ?transit_gateway_configuration
    () =
  { client_vpn_endpoint_id
  ; server_certificate_arn
  ; connection_log_options
  ; dns_servers
  ; vpn_port
  ; description
  ; split_tunnel
  ; dry_run
  ; security_group_ids
  ; vpc_id
  ; self_service_portal
  ; client_connect_options
  ; session_timeout_hours
  ; client_login_banner_options
  ; client_route_enforcement_options
  ; disconnect_on_session_timeout
  ; transit_gateway_configuration
  }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Xml.required
          "ClientVpnEndpointId"
          (Aws.Util.option_bind (Aws.Xml.member "ClientVpnEndpointId" xml) String.parse)
    ; server_certificate_arn =
        Aws.Util.option_bind (Aws.Xml.member "ServerCertificateArn" xml) String.parse
    ; connection_log_options =
        Aws.Util.option_bind
          (Aws.Xml.member "ConnectionLogOptions" xml)
          ConnectionLogOptions.parse
    ; dns_servers =
        Aws.Util.option_bind
          (Aws.Xml.member "DnsServers" xml)
          DnsServersOptionsModifyStructure.parse
    ; vpn_port = Aws.Util.option_bind (Aws.Xml.member "VpnPort" xml) Integer.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; split_tunnel = Aws.Util.option_bind (Aws.Xml.member "SplitTunnel" xml) Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
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
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.split_tunnel (fun f ->
             Aws.Query.Pair ("SplitTunnel", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.vpn_port (fun f ->
             Aws.Query.Pair ("VpnPort", Integer.to_query f))
       ; Aws.Util.option_map v.dns_servers (fun f ->
             Aws.Query.Pair ("DnsServers", DnsServersOptionsModifyStructure.to_query f))
       ; Aws.Util.option_map v.connection_log_options (fun f ->
             Aws.Query.Pair ("ConnectionLogOptions", ConnectionLogOptions.to_query f))
       ; Aws.Util.option_map v.server_certificate_arn (fun f ->
             Aws.Query.Pair ("ServerCertificateArn", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ClientVpnEndpointId", String.to_query v.client_vpn_endpoint_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_configuration (fun f ->
             ( "TransitGatewayConfiguration"
             , TransitGatewayConfigurationInputStructure.to_json f ))
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
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.split_tunnel (fun f -> "SplitTunnel", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.vpn_port (fun f -> "VpnPort", Integer.to_json f)
       ; Aws.Util.option_map v.dns_servers (fun f ->
             "DnsServers", DnsServersOptionsModifyStructure.to_json f)
       ; Aws.Util.option_map v.connection_log_options (fun f ->
             "ConnectionLogOptions", ConnectionLogOptions.to_json f)
       ; Aws.Util.option_map v.server_certificate_arn (fun f ->
             "ServerCertificateArn", String.to_json f)
       ; Some ("ClientVpnEndpointId", String.to_json v.client_vpn_endpoint_id)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ClientVpnEndpointId"))
  ; server_certificate_arn =
      Aws.Util.option_map (Aws.Json.lookup j "ServerCertificateArn") String.of_json
  ; connection_log_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "ConnectionLogOptions")
        ConnectionLogOptions.of_json
  ; dns_servers =
      Aws.Util.option_map
        (Aws.Json.lookup j "DnsServers")
        DnsServersOptionsModifyStructure.of_json
  ; vpn_port = Aws.Util.option_map (Aws.Json.lookup j "VpnPort") Integer.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; split_tunnel = Aws.Util.option_map (Aws.Json.lookup j "SplitTunnel") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
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
  ; transit_gateway_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "TransitGatewayConfiguration")
        TransitGatewayConfigurationInputStructure.of_json
  }
