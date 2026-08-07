open Aws.BaseTypes

type t =
  { enable_acceleration : Boolean.t option
  ; static_routes_only : Boolean.t option
  ; local_ipv4_network_cidr : String.t option
  ; remote_ipv4_network_cidr : String.t option
  ; local_ipv6_network_cidr : String.t option
  ; remote_ipv6_network_cidr : String.t option
  ; outside_ip_address_type : String.t option
  ; transport_transit_gateway_attachment_id : String.t option
  ; tunnel_inside_ip_version : TunnelInsideIpVersion.t option
  ; tunnel_options : TunnelOptionsList.t
  ; tunnel_bandwidth : VpnTunnelBandwidth.t option
  }

let make
    ?enable_acceleration
    ?static_routes_only
    ?local_ipv4_network_cidr
    ?remote_ipv4_network_cidr
    ?local_ipv6_network_cidr
    ?remote_ipv6_network_cidr
    ?outside_ip_address_type
    ?transport_transit_gateway_attachment_id
    ?tunnel_inside_ip_version
    ?(tunnel_options = [])
    ?tunnel_bandwidth
    () =
  { enable_acceleration
  ; static_routes_only
  ; local_ipv4_network_cidr
  ; remote_ipv4_network_cidr
  ; local_ipv6_network_cidr
  ; remote_ipv6_network_cidr
  ; outside_ip_address_type
  ; transport_transit_gateway_attachment_id
  ; tunnel_inside_ip_version
  ; tunnel_options
  ; tunnel_bandwidth
  }

let parse xml =
  Some
    { enable_acceleration =
        Aws.Util.option_bind (Aws.Xml.member "enableAcceleration" xml) Boolean.parse
    ; static_routes_only =
        Aws.Util.option_bind (Aws.Xml.member "staticRoutesOnly" xml) Boolean.parse
    ; local_ipv4_network_cidr =
        Aws.Util.option_bind (Aws.Xml.member "localIpv4NetworkCidr" xml) String.parse
    ; remote_ipv4_network_cidr =
        Aws.Util.option_bind (Aws.Xml.member "remoteIpv4NetworkCidr" xml) String.parse
    ; local_ipv6_network_cidr =
        Aws.Util.option_bind (Aws.Xml.member "localIpv6NetworkCidr" xml) String.parse
    ; remote_ipv6_network_cidr =
        Aws.Util.option_bind (Aws.Xml.member "remoteIpv6NetworkCidr" xml) String.parse
    ; outside_ip_address_type =
        Aws.Util.option_bind (Aws.Xml.member "outsideIpAddressType" xml) String.parse
    ; transport_transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transportTransitGatewayAttachmentId" xml)
          String.parse
    ; tunnel_inside_ip_version =
        Aws.Util.option_bind
          (Aws.Xml.member "tunnelInsideIpVersion" xml)
          TunnelInsideIpVersion.parse
    ; tunnel_options =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "tunnelOptionSet" xml)
             TunnelOptionsList.parse)
    ; tunnel_bandwidth =
        Aws.Util.option_bind
          (Aws.Xml.member "tunnelBandwidth" xml)
          VpnTunnelBandwidth.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.tunnel_bandwidth (fun f ->
             Aws.Query.Pair ("TunnelBandwidth", VpnTunnelBandwidth.to_query f))
       ; Some
           (Aws.Query.Pair ("TunnelOptionSet", TunnelOptionsList.to_query v.tunnel_options))
       ; Aws.Util.option_map v.tunnel_inside_ip_version (fun f ->
             Aws.Query.Pair ("TunnelInsideIpVersion", TunnelInsideIpVersion.to_query f))
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
       ; Aws.Util.option_map v.static_routes_only (fun f ->
             Aws.Query.Pair ("StaticRoutesOnly", Boolean.to_query f))
       ; Aws.Util.option_map v.enable_acceleration (fun f ->
             Aws.Query.Pair ("EnableAcceleration", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.tunnel_bandwidth (fun f ->
             "tunnelBandwidth", VpnTunnelBandwidth.to_json f)
       ; Some ("tunnelOptionSet", TunnelOptionsList.to_json v.tunnel_options)
       ; Aws.Util.option_map v.tunnel_inside_ip_version (fun f ->
             "tunnelInsideIpVersion", TunnelInsideIpVersion.to_json f)
       ; Aws.Util.option_map v.transport_transit_gateway_attachment_id (fun f ->
             "transportTransitGatewayAttachmentId", String.to_json f)
       ; Aws.Util.option_map v.outside_ip_address_type (fun f ->
             "outsideIpAddressType", String.to_json f)
       ; Aws.Util.option_map v.remote_ipv6_network_cidr (fun f ->
             "remoteIpv6NetworkCidr", String.to_json f)
       ; Aws.Util.option_map v.local_ipv6_network_cidr (fun f ->
             "localIpv6NetworkCidr", String.to_json f)
       ; Aws.Util.option_map v.remote_ipv4_network_cidr (fun f ->
             "remoteIpv4NetworkCidr", String.to_json f)
       ; Aws.Util.option_map v.local_ipv4_network_cidr (fun f ->
             "localIpv4NetworkCidr", String.to_json f)
       ; Aws.Util.option_map v.static_routes_only (fun f ->
             "staticRoutesOnly", Boolean.to_json f)
       ; Aws.Util.option_map v.enable_acceleration (fun f ->
             "enableAcceleration", Boolean.to_json f)
       ])

let of_json j =
  { enable_acceleration =
      Aws.Util.option_map (Aws.Json.lookup j "enableAcceleration") Boolean.of_json
  ; static_routes_only =
      Aws.Util.option_map (Aws.Json.lookup j "staticRoutesOnly") Boolean.of_json
  ; local_ipv4_network_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "localIpv4NetworkCidr") String.of_json
  ; remote_ipv4_network_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "remoteIpv4NetworkCidr") String.of_json
  ; local_ipv6_network_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "localIpv6NetworkCidr") String.of_json
  ; remote_ipv6_network_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "remoteIpv6NetworkCidr") String.of_json
  ; outside_ip_address_type =
      Aws.Util.option_map (Aws.Json.lookup j "outsideIpAddressType") String.of_json
  ; transport_transit_gateway_attachment_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "transportTransitGatewayAttachmentId")
        String.of_json
  ; tunnel_inside_ip_version =
      Aws.Util.option_map
        (Aws.Json.lookup j "tunnelInsideIpVersion")
        TunnelInsideIpVersion.of_json
  ; tunnel_options =
      TunnelOptionsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "tunnelOptionSet"))
  ; tunnel_bandwidth =
      Aws.Util.option_map (Aws.Json.lookup j "tunnelBandwidth") VpnTunnelBandwidth.of_json
  }
