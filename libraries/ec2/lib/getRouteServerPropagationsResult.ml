type t = { route_server_propagations : RouteServerPropagationsList.t }

let make ?(route_server_propagations = []) () = { route_server_propagations }

let parse xml =
  Some
    { route_server_propagations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "routeServerPropagationSet" xml)
             RouteServerPropagationsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "RouteServerPropagationSet"
              , RouteServerPropagationsList.to_query v.route_server_propagations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "routeServerPropagationSet"
           , RouteServerPropagationsList.to_json v.route_server_propagations )
       ])

let of_json j =
  { route_server_propagations =
      RouteServerPropagationsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "routeServerPropagationSet"))
  }
