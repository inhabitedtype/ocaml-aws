type t = { vpn_concentrator : VpnConcentrator.t option }

let make ?vpn_concentrator () = { vpn_concentrator }

let parse xml =
  Some
    { vpn_concentrator =
        Aws.Util.option_bind (Aws.Xml.member "vpnConcentrator" xml) VpnConcentrator.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpn_concentrator (fun f ->
             Aws.Query.Pair ("VpnConcentrator", VpnConcentrator.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpn_concentrator (fun f ->
             "vpnConcentrator", VpnConcentrator.to_json f)
       ])

let of_json j =
  { vpn_concentrator =
      Aws.Util.option_map (Aws.Json.lookup j "vpnConcentrator") VpnConcentrator.of_json
  }
