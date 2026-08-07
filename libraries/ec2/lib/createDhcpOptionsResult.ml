type t = { dhcp_options : DhcpOptions.t option }

let make ?dhcp_options () = { dhcp_options }

let parse xml =
  Some
    { dhcp_options =
        Aws.Util.option_bind (Aws.Xml.member "dhcpOptions" xml) DhcpOptions.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dhcp_options (fun f ->
             Aws.Query.Pair ("DhcpOptions", DhcpOptions.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dhcp_options (fun f ->
             "dhcpOptions", DhcpOptions.to_json f)
       ])

let of_json j =
  { dhcp_options =
      Aws.Util.option_map (Aws.Json.lookup j "dhcpOptions") DhcpOptions.of_json
  }
