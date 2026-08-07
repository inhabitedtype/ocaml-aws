open Aws.BaseTypes

type t =
  { vpn_connection_id : String.t
  ; local_ipv4_network_cidr : String.t option
  ; remote_ipv4_network_cidr : String.t option
  ; local_ipv6_network_cidr : String.t option
  ; remote_ipv6_network_cidr : String.t option
  ; tunnel_bandwidth : VpnTunnelBandwidth.t option
  ; dry_run : Boolean.t option
  }

let make
    ~vpn_connection_id
    ?local_ipv4_network_cidr
    ?remote_ipv4_network_cidr
    ?local_ipv6_network_cidr
    ?remote_ipv6_network_cidr
    ?tunnel_bandwidth
    ?dry_run
    () =
  { vpn_connection_id
  ; local_ipv4_network_cidr
  ; remote_ipv4_network_cidr
  ; local_ipv6_network_cidr
  ; remote_ipv6_network_cidr
  ; tunnel_bandwidth
  ; dry_run
  }

let parse xml =
  Some
    { vpn_connection_id =
        Aws.Xml.required
          "VpnConnectionId"
          (Aws.Util.option_bind (Aws.Xml.member "VpnConnectionId" xml) String.parse)
    ; local_ipv4_network_cidr =
        Aws.Util.option_bind (Aws.Xml.member "LocalIpv4NetworkCidr" xml) String.parse
    ; remote_ipv4_network_cidr =
        Aws.Util.option_bind (Aws.Xml.member "RemoteIpv4NetworkCidr" xml) String.parse
    ; local_ipv6_network_cidr =
        Aws.Util.option_bind (Aws.Xml.member "LocalIpv6NetworkCidr" xml) String.parse
    ; remote_ipv6_network_cidr =
        Aws.Util.option_bind (Aws.Xml.member "RemoteIpv6NetworkCidr" xml) String.parse
    ; tunnel_bandwidth =
        Aws.Util.option_bind
          (Aws.Xml.member "TunnelBandwidth" xml)
          VpnTunnelBandwidth.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.tunnel_bandwidth (fun f ->
             Aws.Query.Pair ("TunnelBandwidth", VpnTunnelBandwidth.to_query f))
       ; Aws.Util.option_map v.remote_ipv6_network_cidr (fun f ->
             Aws.Query.Pair ("RemoteIpv6NetworkCidr", String.to_query f))
       ; Aws.Util.option_map v.local_ipv6_network_cidr (fun f ->
             Aws.Query.Pair ("LocalIpv6NetworkCidr", String.to_query f))
       ; Aws.Util.option_map v.remote_ipv4_network_cidr (fun f ->
             Aws.Query.Pair ("RemoteIpv4NetworkCidr", String.to_query f))
       ; Aws.Util.option_map v.local_ipv4_network_cidr (fun f ->
             Aws.Query.Pair ("LocalIpv4NetworkCidr", String.to_query f))
       ; Some (Aws.Query.Pair ("VpnConnectionId", String.to_query v.vpn_connection_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.tunnel_bandwidth (fun f ->
             "TunnelBandwidth", VpnTunnelBandwidth.to_json f)
       ; Aws.Util.option_map v.remote_ipv6_network_cidr (fun f ->
             "RemoteIpv6NetworkCidr", String.to_json f)
       ; Aws.Util.option_map v.local_ipv6_network_cidr (fun f ->
             "LocalIpv6NetworkCidr", String.to_json f)
       ; Aws.Util.option_map v.remote_ipv4_network_cidr (fun f ->
             "RemoteIpv4NetworkCidr", String.to_json f)
       ; Aws.Util.option_map v.local_ipv4_network_cidr (fun f ->
             "LocalIpv4NetworkCidr", String.to_json f)
       ; Some ("VpnConnectionId", String.to_json v.vpn_connection_id)
       ])

let of_json j =
  { vpn_connection_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpnConnectionId"))
  ; local_ipv4_network_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "LocalIpv4NetworkCidr") String.of_json
  ; remote_ipv4_network_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "RemoteIpv4NetworkCidr") String.of_json
  ; local_ipv6_network_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "LocalIpv6NetworkCidr") String.of_json
  ; remote_ipv6_network_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "RemoteIpv6NetworkCidr") String.of_json
  ; tunnel_bandwidth =
      Aws.Util.option_map (Aws.Json.lookup j "TunnelBandwidth") VpnTunnelBandwidth.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
