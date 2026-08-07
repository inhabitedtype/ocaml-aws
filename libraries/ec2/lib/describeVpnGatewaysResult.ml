type t = { vpn_gateways : VpnGatewayList.t }

let make ?(vpn_gateways = []) () = { vpn_gateways }

let parse xml =
  Some
    { vpn_gateways =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "vpnGatewaySet" xml) VpnGatewayList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("VpnGatewaySet", VpnGatewayList.to_query v.vpn_gateways)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("vpnGatewaySet", VpnGatewayList.to_json v.vpn_gateways) ])

let of_json j =
  { vpn_gateways =
      VpnGatewayList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpnGatewaySet"))
  }
