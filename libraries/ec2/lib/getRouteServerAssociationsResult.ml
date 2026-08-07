type t = { route_server_associations : RouteServerAssociationsList.t }

let make ?(route_server_associations = []) () = { route_server_associations }

let parse xml =
  Some
    { route_server_associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "routeServerAssociationSet" xml)
             RouteServerAssociationsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "RouteServerAssociationSet"
              , RouteServerAssociationsList.to_query v.route_server_associations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "routeServerAssociationSet"
           , RouteServerAssociationsList.to_json v.route_server_associations )
       ])

let of_json j =
  { route_server_associations =
      RouteServerAssociationsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "routeServerAssociationSet"))
  }
