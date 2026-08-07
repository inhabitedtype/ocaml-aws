open Aws.BaseTypes

type t =
  { vpn_connection_id : String.t option
  ; transit_gateway_id : String.t option
  ; customer_gateway_id : String.t option
  ; vpn_gateway_id : String.t option
  ; vpn_tunnel_outside_ip_address : String.t option
  ; maintenance_details : MaintenanceDetails.t option
  }

let make
    ?vpn_connection_id
    ?transit_gateway_id
    ?customer_gateway_id
    ?vpn_gateway_id
    ?vpn_tunnel_outside_ip_address
    ?maintenance_details
    () =
  { vpn_connection_id
  ; transit_gateway_id
  ; customer_gateway_id
  ; vpn_gateway_id
  ; vpn_tunnel_outside_ip_address
  ; maintenance_details
  }

let parse xml =
  Some
    { vpn_connection_id =
        Aws.Util.option_bind (Aws.Xml.member "vpnConnectionId" xml) String.parse
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    ; customer_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "customerGatewayId" xml) String.parse
    ; vpn_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "vpnGatewayId" xml) String.parse
    ; vpn_tunnel_outside_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "vpnTunnelOutsideIpAddress" xml) String.parse
    ; maintenance_details =
        Aws.Util.option_bind
          (Aws.Xml.member "maintenanceDetails" xml)
          MaintenanceDetails.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maintenance_details (fun f ->
             Aws.Query.Pair ("MaintenanceDetails", MaintenanceDetails.to_query f))
       ; Aws.Util.option_map v.vpn_tunnel_outside_ip_address (fun f ->
             Aws.Query.Pair ("VpnTunnelOutsideIpAddress", String.to_query f))
       ; Aws.Util.option_map v.vpn_gateway_id (fun f ->
             Aws.Query.Pair ("VpnGatewayId", String.to_query f))
       ; Aws.Util.option_map v.customer_gateway_id (fun f ->
             Aws.Query.Pair ("CustomerGatewayId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.vpn_connection_id (fun f ->
             Aws.Query.Pair ("VpnConnectionId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maintenance_details (fun f ->
             "maintenanceDetails", MaintenanceDetails.to_json f)
       ; Aws.Util.option_map v.vpn_tunnel_outside_ip_address (fun f ->
             "vpnTunnelOutsideIpAddress", String.to_json f)
       ; Aws.Util.option_map v.vpn_gateway_id (fun f -> "vpnGatewayId", String.to_json f)
       ; Aws.Util.option_map v.customer_gateway_id (fun f ->
             "customerGatewayId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "transitGatewayId", String.to_json f)
       ; Aws.Util.option_map v.vpn_connection_id (fun f ->
             "vpnConnectionId", String.to_json f)
       ])

let of_json j =
  { vpn_connection_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpnConnectionId") String.of_json
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayId") String.of_json
  ; customer_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "customerGatewayId") String.of_json
  ; vpn_gateway_id = Aws.Util.option_map (Aws.Json.lookup j "vpnGatewayId") String.of_json
  ; vpn_tunnel_outside_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "vpnTunnelOutsideIpAddress") String.of_json
  ; maintenance_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "maintenanceDetails")
        MaintenanceDetails.of_json
  }
