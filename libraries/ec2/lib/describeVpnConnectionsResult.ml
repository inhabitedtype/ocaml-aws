type t = { vpn_connections : VpnConnectionList.t }

let make ?(vpn_connections = []) () = { vpn_connections }

let parse xml =
  Some
    { vpn_connections =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "vpnConnectionSet" xml)
             VpnConnectionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("VpnConnectionSet", VpnConnectionList.to_query v.vpn_connections))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("vpnConnectionSet", VpnConnectionList.to_json v.vpn_connections) ])

let of_json j =
  { vpn_connections =
      VpnConnectionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "vpnConnectionSet"))
  }
