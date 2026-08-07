type t = { route_server_endpoint : RouteServerEndpoint.t option }

let make ?route_server_endpoint () = { route_server_endpoint }

let parse xml =
  Some
    { route_server_endpoint =
        Aws.Util.option_bind
          (Aws.Xml.member "routeServerEndpoint" xml)
          RouteServerEndpoint.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route_server_endpoint (fun f ->
             Aws.Query.Pair ("RouteServerEndpoint", RouteServerEndpoint.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route_server_endpoint (fun f ->
             "routeServerEndpoint", RouteServerEndpoint.to_json f)
       ])

let of_json j =
  { route_server_endpoint =
      Aws.Util.option_map
        (Aws.Json.lookup j "routeServerEndpoint")
        RouteServerEndpoint.of_json
  }
