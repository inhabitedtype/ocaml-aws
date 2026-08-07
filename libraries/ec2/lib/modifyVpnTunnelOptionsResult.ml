type t = { vpn_connection : VpnConnection.t option }

let make ?vpn_connection () = { vpn_connection }

let parse xml =
  Some
    { vpn_connection =
        Aws.Util.option_bind (Aws.Xml.member "vpnConnection" xml) VpnConnection.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpn_connection (fun f ->
             Aws.Query.Pair ("VpnConnection", VpnConnection.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpn_connection (fun f ->
             "vpnConnection", VpnConnection.to_json f)
       ])

let of_json j =
  { vpn_connection =
      Aws.Util.option_map (Aws.Json.lookup j "vpnConnection") VpnConnection.of_json
  }
