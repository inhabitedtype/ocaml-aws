type t = { route_server_propagation : RouteServerPropagation.t option }

let make ?route_server_propagation () = { route_server_propagation }

let parse xml =
  Some
    { route_server_propagation =
        Aws.Util.option_bind
          (Aws.Xml.member "routeServerPropagation" xml)
          RouteServerPropagation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route_server_propagation (fun f ->
             Aws.Query.Pair ("RouteServerPropagation", RouteServerPropagation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route_server_propagation (fun f ->
             "routeServerPropagation", RouteServerPropagation.to_json f)
       ])

let of_json j =
  { route_server_propagation =
      Aws.Util.option_map
        (Aws.Json.lookup j "routeServerPropagation")
        RouteServerPropagation.of_json
  }
