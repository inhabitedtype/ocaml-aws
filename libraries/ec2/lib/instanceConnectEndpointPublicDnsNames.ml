type t =
  { ipv4 : InstanceConnectEndpointDnsNames.t option
  ; dualstack : InstanceConnectEndpointDnsNames.t option
  }

let make ?ipv4 ?dualstack () = { ipv4; dualstack }

let parse xml =
  Some
    { ipv4 =
        Aws.Util.option_bind
          (Aws.Xml.member "ipv4" xml)
          InstanceConnectEndpointDnsNames.parse
    ; dualstack =
        Aws.Util.option_bind
          (Aws.Xml.member "dualstack" xml)
          InstanceConnectEndpointDnsNames.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dualstack (fun f ->
             Aws.Query.Pair ("Dualstack", InstanceConnectEndpointDnsNames.to_query f))
       ; Aws.Util.option_map v.ipv4 (fun f ->
             Aws.Query.Pair ("Ipv4", InstanceConnectEndpointDnsNames.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dualstack (fun f ->
             "dualstack", InstanceConnectEndpointDnsNames.to_json f)
       ; Aws.Util.option_map v.ipv4 (fun f ->
             "ipv4", InstanceConnectEndpointDnsNames.to_json f)
       ])

let of_json j =
  { ipv4 =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipv4")
        InstanceConnectEndpointDnsNames.of_json
  ; dualstack =
      Aws.Util.option_map
        (Aws.Json.lookup j "dualstack")
        InstanceConnectEndpointDnsNames.of_json
  }
