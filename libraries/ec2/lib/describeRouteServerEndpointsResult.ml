open Aws.BaseTypes

type t =
  { route_server_endpoints : RouteServerEndpointsList.t
  ; next_token : String.t option
  }

let make ?(route_server_endpoints = []) ?next_token () =
  { route_server_endpoints; next_token }

let parse xml =
  Some
    { route_server_endpoints =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "routeServerEndpointSet" xml)
             RouteServerEndpointsList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "RouteServerEndpointSet"
              , RouteServerEndpointsList.to_query v.route_server_endpoints ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "routeServerEndpointSet"
           , RouteServerEndpointsList.to_json v.route_server_endpoints )
       ])

let of_json j =
  { route_server_endpoints =
      RouteServerEndpointsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "routeServerEndpointSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
