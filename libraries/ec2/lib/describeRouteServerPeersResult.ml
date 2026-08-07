open Aws.BaseTypes

type t =
  { route_server_peers : RouteServerPeersList.t
  ; next_token : String.t option
  }

let make ?(route_server_peers = []) ?next_token () = { route_server_peers; next_token }

let parse xml =
  Some
    { route_server_peers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "routeServerPeerSet" xml)
             RouteServerPeersList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("RouteServerPeerSet", RouteServerPeersList.to_query v.route_server_peers))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("routeServerPeerSet", RouteServerPeersList.to_json v.route_server_peers)
       ])

let of_json j =
  { route_server_peers =
      RouteServerPeersList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "routeServerPeerSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
