open Aws.BaseTypes

type t =
  { enable_acceleration : Boolean.t option
  ; tunnel_inside_ip_version : TunnelInsideIpVersion.t option
  ; tunnel_options : VpnTunnelOptionsSpecificationsList.t
  ; local_ipv4_network_cidr : String.t option
  ; remote_ipv4_network_cidr : String.t option
  ; local_ipv6_network_cidr : String.t option
  ; remote_ipv6_network_cidr : String.t option
  ; outside_ip_address_type : String.t option
  ; transport_transit_gateway_attachment_id : String.t option
  ; tunnel_bandwidth : VpnTunnelBandwidth.t option
  ; static_routes_only : Boolean.t option
  }

let make
    ?enable_acceleration
    ?tunnel_inside_ip_version
    ?(tunnel_options = [])
    ?local_ipv4_network_cidr
    ?remote_ipv4_network_cidr
    ?local_ipv6_network_cidr
    ?remote_ipv6_network_cidr
    ?outside_ip_address_type
    ?transport_transit_gateway_attachment_id
    ?tunnel_bandwidth
    ?static_routes_only
    () =
  { enable_acceleration
  ; tunnel_inside_ip_version
  ; tunnel_options
  ; local_ipv4_network_cidr
  ; remote_ipv4_network_cidr
  ; local_ipv6_network_cidr
  ; remote_ipv6_network_cidr
  ; outside_ip_address_type
  ; transport_transit_gateway_attachment_id
  ; tunnel_bandwidth
  ; static_routes_only
  }

let parse xml =
  Some
    { enable_acceleration =
        Aws.Util.option_bind (Aws.Xml.member "EnableAcceleration" xml) Boolean.parse
    ; tunnel_inside_ip_version =
        Aws.Util.option_bind
          (Aws.Xml.member "TunnelInsideIpVersion" xml)
          TunnelInsideIpVersion.parse
    ; tunnel_options =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TunnelOptions" xml)
             VpnTunnelOptionsSpecificationsList.parse)
    ; local_ipv4_network_cidr =
        Aws.Util.option_bind (Aws.Xml.member "LocalIpv4NetworkCidr" xml) String.parse
    ; remote_ipv4_network_cidr =
        Aws.Util.option_bind (Aws.Xml.member "RemoteIpv4NetworkCidr" xml) String.parse
    ; local_ipv6_network_cidr =
        Aws.Util.option_bind (Aws.Xml.member "LocalIpv6NetworkCidr" xml) String.parse
    ; remote_ipv6_network_cidr =
        Aws.Util.option_bind (Aws.Xml.member "RemoteIpv6NetworkCidr" xml) String.parse
    ; outside_ip_address_type =
        Aws.Util.option_bind (Aws.Xml.member "OutsideIpAddressType" xml) String.parse
    ; transport_transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "TransportTransitGatewayAttachmentId" xml)
          String.parse
    ; tunnel_bandwidth =
        Aws.Util.option_bind
          (Aws.Xml.member "TunnelBandwidth" xml)
          VpnTunnelBandwidth.parse
    ; static_routes_only =
        Aws.Util.option_bind (Aws.Xml.member "staticRoutesOnly" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.static_routes_only (fun f ->
             Aws.Query.Pair ("StaticRoutesOnly", Boolean.to_query f))
       ; Aws.Util.option_map v.tunnel_bandwidth (fun f ->
             Aws.Query.Pair ("TunnelBandwidth", VpnTunnelBandwidth.to_query f))
       ; Aws.Util.option_map v.transport_transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("TransportTransitGatewayAttachmentId", String.to_query f))
       ; Aws.Util.option_map v.outside_ip_address_type (fun f ->
             Aws.Query.Pair ("OutsideIpAddressType", String.to_query f))
       ; Aws.Util.option_map v.remote_ipv6_network_cidr (fun f ->
             Aws.Query.Pair ("RemoteIpv6NetworkCidr", String.to_query f))
       ; Aws.Util.option_map v.local_ipv6_network_cidr (fun f ->
             Aws.Query.Pair ("LocalIpv6NetworkCidr", String.to_query f))
       ; Aws.Util.option_map v.remote_ipv4_network_cidr (fun f ->
             Aws.Query.Pair ("RemoteIpv4NetworkCidr", String.to_query f))
       ; Aws.Util.option_map v.local_ipv4_network_cidr (fun f ->
             Aws.Query.Pair ("LocalIpv4NetworkCidr", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TunnelOptions"
              , VpnTunnelOptionsSpecificationsList.to_query v.tunnel_options ))
       ; Aws.Util.option_map v.tunnel_inside_ip_version (fun f ->
             Aws.Query.Pair ("TunnelInsideIpVersion", TunnelInsideIpVersion.to_query f))
       ; Aws.Util.option_map v.enable_acceleration (fun f ->
             Aws.Query.Pair ("EnableAcceleration", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.static_routes_only (fun f ->
             "staticRoutesOnly", Boolean.to_json f)
       ; Aws.Util.option_map v.tunnel_bandwidth (fun f ->
             "TunnelBandwidth", VpnTunnelBandwidth.to_json f)
       ; Aws.Util.option_map v.transport_transit_gateway_attachment_id (fun f ->
             "TransportTransitGatewayAttachmentId", String.to_json f)
       ; Aws.Util.option_map v.outside_ip_address_type (fun f ->
             "OutsideIpAddressType", String.to_json f)
       ; Aws.Util.option_map v.remote_ipv6_network_cidr (fun f ->
             "RemoteIpv6NetworkCidr", String.to_json f)
       ; Aws.Util.option_map v.local_ipv6_network_cidr (fun f ->
             "LocalIpv6NetworkCidr", String.to_json f)
       ; Aws.Util.option_map v.remote_ipv4_network_cidr (fun f ->
             "RemoteIpv4NetworkCidr", String.to_json f)
       ; Aws.Util.option_map v.local_ipv4_network_cidr (fun f ->
             "LocalIpv4NetworkCidr", String.to_json f)
       ; Some
           ("TunnelOptions", VpnTunnelOptionsSpecificationsList.to_json v.tunnel_options)
       ; Aws.Util.option_map v.tunnel_inside_ip_version (fun f ->
             "TunnelInsideIpVersion", TunnelInsideIpVersion.to_json f)
       ; Aws.Util.option_map v.enable_acceleration (fun f ->
             "EnableAcceleration", Boolean.to_json f)
       ])

let of_json j =
  { enable_acceleration =
      Aws.Util.option_map (Aws.Json.lookup j "EnableAcceleration") Boolean.of_json
  ; tunnel_inside_ip_version =
      Aws.Util.option_map
        (Aws.Json.lookup j "TunnelInsideIpVersion")
        TunnelInsideIpVersion.of_json
  ; tunnel_options =
      VpnTunnelOptionsSpecificationsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TunnelOptions"))
  ; local_ipv4_network_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "LocalIpv4NetworkCidr") String.of_json
  ; remote_ipv4_network_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "RemoteIpv4NetworkCidr") String.of_json
  ; local_ipv6_network_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "LocalIpv6NetworkCidr") String.of_json
  ; remote_ipv6_network_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "RemoteIpv6NetworkCidr") String.of_json
  ; outside_ip_address_type =
      Aws.Util.option_map (Aws.Json.lookup j "OutsideIpAddressType") String.of_json
  ; transport_transit_gateway_attachment_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "TransportTransitGatewayAttachmentId")
        String.of_json
  ; tunnel_bandwidth =
      Aws.Util.option_map (Aws.Json.lookup j "TunnelBandwidth") VpnTunnelBandwidth.of_json
  ; static_routes_only =
      Aws.Util.option_map (Aws.Json.lookup j "staticRoutesOnly") Boolean.of_json
  }
