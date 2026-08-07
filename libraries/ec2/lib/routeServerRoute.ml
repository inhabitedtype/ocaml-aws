open Aws.BaseTypes

type t =
  { route_server_endpoint_id : String.t option
  ; route_server_peer_id : String.t option
  ; route_installation_details : RouteServerRouteInstallationDetails.t
  ; route_status : RouteServerRouteStatus.t option
  ; prefix : String.t option
  ; as_paths : AsPath.t
  ; med : Integer.t option
  ; next_hop_ip : String.t option
  }

let make
    ?route_server_endpoint_id
    ?route_server_peer_id
    ?(route_installation_details = [])
    ?route_status
    ?prefix
    ?(as_paths = [])
    ?med
    ?next_hop_ip
    () =
  { route_server_endpoint_id
  ; route_server_peer_id
  ; route_installation_details
  ; route_status
  ; prefix
  ; as_paths
  ; med
  ; next_hop_ip
  }

let parse xml =
  Some
    { route_server_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "routeServerEndpointId" xml) String.parse
    ; route_server_peer_id =
        Aws.Util.option_bind (Aws.Xml.member "routeServerPeerId" xml) String.parse
    ; route_installation_details =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "routeInstallationDetailSet" xml)
             RouteServerRouteInstallationDetails.parse)
    ; route_status =
        Aws.Util.option_bind
          (Aws.Xml.member "routeStatus" xml)
          RouteServerRouteStatus.parse
    ; prefix = Aws.Util.option_bind (Aws.Xml.member "prefix" xml) String.parse
    ; as_paths =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "asPathSet" xml) AsPath.parse)
    ; med = Aws.Util.option_bind (Aws.Xml.member "med" xml) Integer.parse
    ; next_hop_ip = Aws.Util.option_bind (Aws.Xml.member "nextHopIp" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_hop_ip (fun f ->
             Aws.Query.Pair ("NextHopIp", String.to_query f))
       ; Aws.Util.option_map v.med (fun f -> Aws.Query.Pair ("Med", Integer.to_query f))
       ; Some (Aws.Query.Pair ("AsPathSet", AsPath.to_query v.as_paths))
       ; Aws.Util.option_map v.prefix (fun f ->
             Aws.Query.Pair ("Prefix", String.to_query f))
       ; Aws.Util.option_map v.route_status (fun f ->
             Aws.Query.Pair ("RouteStatus", RouteServerRouteStatus.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "RouteInstallationDetailSet"
              , RouteServerRouteInstallationDetails.to_query v.route_installation_details
              ))
       ; Aws.Util.option_map v.route_server_peer_id (fun f ->
             Aws.Query.Pair ("RouteServerPeerId", String.to_query f))
       ; Aws.Util.option_map v.route_server_endpoint_id (fun f ->
             Aws.Query.Pair ("RouteServerEndpointId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_hop_ip (fun f -> "nextHopIp", String.to_json f)
       ; Aws.Util.option_map v.med (fun f -> "med", Integer.to_json f)
       ; Some ("asPathSet", AsPath.to_json v.as_paths)
       ; Aws.Util.option_map v.prefix (fun f -> "prefix", String.to_json f)
       ; Aws.Util.option_map v.route_status (fun f ->
             "routeStatus", RouteServerRouteStatus.to_json f)
       ; Some
           ( "routeInstallationDetailSet"
           , RouteServerRouteInstallationDetails.to_json v.route_installation_details )
       ; Aws.Util.option_map v.route_server_peer_id (fun f ->
             "routeServerPeerId", String.to_json f)
       ; Aws.Util.option_map v.route_server_endpoint_id (fun f ->
             "routeServerEndpointId", String.to_json f)
       ])

let of_json j =
  { route_server_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "routeServerEndpointId") String.of_json
  ; route_server_peer_id =
      Aws.Util.option_map (Aws.Json.lookup j "routeServerPeerId") String.of_json
  ; route_installation_details =
      RouteServerRouteInstallationDetails.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "routeInstallationDetailSet"))
  ; route_status =
      Aws.Util.option_map (Aws.Json.lookup j "routeStatus") RouteServerRouteStatus.of_json
  ; prefix = Aws.Util.option_map (Aws.Json.lookup j "prefix") String.of_json
  ; as_paths = AsPath.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "asPathSet"))
  ; med = Aws.Util.option_map (Aws.Json.lookup j "med") Integer.of_json
  ; next_hop_ip = Aws.Util.option_map (Aws.Json.lookup j "nextHopIp") String.of_json
  }
