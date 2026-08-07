type t = { active_vpn_tunnel_status : ActiveVpnTunnelStatus.t option }

let make ?active_vpn_tunnel_status () = { active_vpn_tunnel_status }

let parse xml =
  Some
    { active_vpn_tunnel_status =
        Aws.Util.option_bind
          (Aws.Xml.member "activeVpnTunnelStatus" xml)
          ActiveVpnTunnelStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.active_vpn_tunnel_status (fun f ->
             Aws.Query.Pair ("ActiveVpnTunnelStatus", ActiveVpnTunnelStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.active_vpn_tunnel_status (fun f ->
             "activeVpnTunnelStatus", ActiveVpnTunnelStatus.to_json f)
       ])

let of_json j =
  { active_vpn_tunnel_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "activeVpnTunnelStatus")
        ActiveVpnTunnelStatus.of_json
  }
