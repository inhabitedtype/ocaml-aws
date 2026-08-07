type t = { route_server : RouteServer.t option }

let make ?route_server () = { route_server }

let parse xml =
  Some
    { route_server =
        Aws.Util.option_bind (Aws.Xml.member "routeServer" xml) RouteServer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route_server (fun f ->
             Aws.Query.Pair ("RouteServer", RouteServer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route_server (fun f ->
             "routeServer", RouteServer.to_json f)
       ])

let of_json j =
  { route_server =
      Aws.Util.option_map (Aws.Json.lookup j "routeServer") RouteServer.of_json
  }
