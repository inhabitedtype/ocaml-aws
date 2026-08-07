type t = { route_server_association : RouteServerAssociation.t option }

let make ?route_server_association () = { route_server_association }

let parse xml =
  Some
    { route_server_association =
        Aws.Util.option_bind
          (Aws.Xml.member "routeServerAssociation" xml)
          RouteServerAssociation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route_server_association (fun f ->
             Aws.Query.Pair ("RouteServerAssociation", RouteServerAssociation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route_server_association (fun f ->
             "routeServerAssociation", RouteServerAssociation.to_json f)
       ])

let of_json j =
  { route_server_association =
      Aws.Util.option_map
        (Aws.Json.lookup j "routeServerAssociation")
        RouteServerAssociation.of_json
  }
