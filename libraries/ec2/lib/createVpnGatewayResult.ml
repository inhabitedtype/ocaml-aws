type t = { vpn_gateway : VpnGateway.t option }

let make ?vpn_gateway () = { vpn_gateway }

let parse xml =
  Some
    { vpn_gateway =
        Aws.Util.option_bind (Aws.Xml.member "vpnGateway" xml) VpnGateway.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpn_gateway (fun f ->
             Aws.Query.Pair ("VpnGateway", VpnGateway.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpn_gateway (fun f -> "vpnGateway", VpnGateway.to_json f) ])

let of_json j =
  { vpn_gateway = Aws.Util.option_map (Aws.Json.lookup j "vpnGateway") VpnGateway.of_json
  }
