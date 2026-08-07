type t = { d_b_proxy_endpoint : DBProxyEndpoint.t option }

let make ?d_b_proxy_endpoint () = { d_b_proxy_endpoint }

let parse xml =
  Some
    { d_b_proxy_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "DBProxyEndpoint" xml) DBProxyEndpoint.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_proxy_endpoint (fun f ->
             Aws.Query.Pair ("DBProxyEndpoint", DBProxyEndpoint.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_proxy_endpoint (fun f ->
             "DBProxyEndpoint", DBProxyEndpoint.to_json f)
       ])

let of_json j =
  { d_b_proxy_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "DBProxyEndpoint") DBProxyEndpoint.of_json
  }
