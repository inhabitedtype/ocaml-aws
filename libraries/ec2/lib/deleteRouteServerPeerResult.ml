type t = { route_server_peer : RouteServerPeer.t option }

let make ?route_server_peer () = { route_server_peer }

let parse xml =
  Some
    { route_server_peer =
        Aws.Util.option_bind (Aws.Xml.member "routeServerPeer" xml) RouteServerPeer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route_server_peer (fun f ->
             Aws.Query.Pair ("RouteServerPeer", RouteServerPeer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route_server_peer (fun f ->
             "routeServerPeer", RouteServerPeer.to_json f)
       ])

let of_json j =
  { route_server_peer =
      Aws.Util.option_map (Aws.Json.lookup j "routeServerPeer") RouteServerPeer.of_json
  }
