open Aws.BaseTypes

type t =
  { route_server_peer_id : String.t option
  ; route_server_endpoint_id : String.t option
  ; route_server_id : String.t option
  ; vpc_id : String.t option
  ; subnet_id : String.t option
  ; state : RouteServerPeerState.t option
  ; failure_reason : String.t option
  ; endpoint_eni_id : String.t option
  ; endpoint_eni_address : String.t option
  ; peer_address : String.t option
  ; bgp_options : RouteServerBgpOptions.t option
  ; bgp_status : RouteServerBgpStatus.t option
  ; bfd_status : RouteServerBfdStatus.t option
  ; tags : TagList.t
  }

let make
    ?route_server_peer_id
    ?route_server_endpoint_id
    ?route_server_id
    ?vpc_id
    ?subnet_id
    ?state
    ?failure_reason
    ?endpoint_eni_id
    ?endpoint_eni_address
    ?peer_address
    ?bgp_options
    ?bgp_status
    ?bfd_status
    ?(tags = [])
    () =
  { route_server_peer_id
  ; route_server_endpoint_id
  ; route_server_id
  ; vpc_id
  ; subnet_id
  ; state
  ; failure_reason
  ; endpoint_eni_id
  ; endpoint_eni_address
  ; peer_address
  ; bgp_options
  ; bgp_status
  ; bfd_status
  ; tags
  }

let parse xml =
  Some
    { route_server_peer_id =
        Aws.Util.option_bind (Aws.Xml.member "routeServerPeerId" xml) String.parse
    ; route_server_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "routeServerEndpointId" xml) String.parse
    ; route_server_id =
        Aws.Util.option_bind (Aws.Xml.member "routeServerId" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) RouteServerPeerState.parse
    ; failure_reason =
        Aws.Util.option_bind (Aws.Xml.member "failureReason" xml) String.parse
    ; endpoint_eni_id =
        Aws.Util.option_bind (Aws.Xml.member "endpointEniId" xml) String.parse
    ; endpoint_eni_address =
        Aws.Util.option_bind (Aws.Xml.member "endpointEniAddress" xml) String.parse
    ; peer_address = Aws.Util.option_bind (Aws.Xml.member "peerAddress" xml) String.parse
    ; bgp_options =
        Aws.Util.option_bind (Aws.Xml.member "bgpOptions" xml) RouteServerBgpOptions.parse
    ; bgp_status =
        Aws.Util.option_bind (Aws.Xml.member "bgpStatus" xml) RouteServerBgpStatus.parse
    ; bfd_status =
        Aws.Util.option_bind (Aws.Xml.member "bfdStatus" xml) RouteServerBfdStatus.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.bfd_status (fun f ->
             Aws.Query.Pair ("BfdStatus", RouteServerBfdStatus.to_query f))
       ; Aws.Util.option_map v.bgp_status (fun f ->
             Aws.Query.Pair ("BgpStatus", RouteServerBgpStatus.to_query f))
       ; Aws.Util.option_map v.bgp_options (fun f ->
             Aws.Query.Pair ("BgpOptions", RouteServerBgpOptions.to_query f))
       ; Aws.Util.option_map v.peer_address (fun f ->
             Aws.Query.Pair ("PeerAddress", String.to_query f))
       ; Aws.Util.option_map v.endpoint_eni_address (fun f ->
             Aws.Query.Pair ("EndpointEniAddress", String.to_query f))
       ; Aws.Util.option_map v.endpoint_eni_id (fun f ->
             Aws.Query.Pair ("EndpointEniId", String.to_query f))
       ; Aws.Util.option_map v.failure_reason (fun f ->
             Aws.Query.Pair ("FailureReason", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", RouteServerPeerState.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.route_server_id (fun f ->
             Aws.Query.Pair ("RouteServerId", String.to_query f))
       ; Aws.Util.option_map v.route_server_endpoint_id (fun f ->
             Aws.Query.Pair ("RouteServerEndpointId", String.to_query f))
       ; Aws.Util.option_map v.route_server_peer_id (fun f ->
             Aws.Query.Pair ("RouteServerPeerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.bfd_status (fun f ->
             "bfdStatus", RouteServerBfdStatus.to_json f)
       ; Aws.Util.option_map v.bgp_status (fun f ->
             "bgpStatus", RouteServerBgpStatus.to_json f)
       ; Aws.Util.option_map v.bgp_options (fun f ->
             "bgpOptions", RouteServerBgpOptions.to_json f)
       ; Aws.Util.option_map v.peer_address (fun f -> "peerAddress", String.to_json f)
       ; Aws.Util.option_map v.endpoint_eni_address (fun f ->
             "endpointEniAddress", String.to_json f)
       ; Aws.Util.option_map v.endpoint_eni_id (fun f ->
             "endpointEniId", String.to_json f)
       ; Aws.Util.option_map v.failure_reason (fun f -> "failureReason", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", RouteServerPeerState.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.route_server_id (fun f ->
             "routeServerId", String.to_json f)
       ; Aws.Util.option_map v.route_server_endpoint_id (fun f ->
             "routeServerEndpointId", String.to_json f)
       ; Aws.Util.option_map v.route_server_peer_id (fun f ->
             "routeServerPeerId", String.to_json f)
       ])

let of_json j =
  { route_server_peer_id =
      Aws.Util.option_map (Aws.Json.lookup j "routeServerPeerId") String.of_json
  ; route_server_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "routeServerEndpointId") String.of_json
  ; route_server_id =
      Aws.Util.option_map (Aws.Json.lookup j "routeServerId") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") RouteServerPeerState.of_json
  ; failure_reason =
      Aws.Util.option_map (Aws.Json.lookup j "failureReason") String.of_json
  ; endpoint_eni_id =
      Aws.Util.option_map (Aws.Json.lookup j "endpointEniId") String.of_json
  ; endpoint_eni_address =
      Aws.Util.option_map (Aws.Json.lookup j "endpointEniAddress") String.of_json
  ; peer_address = Aws.Util.option_map (Aws.Json.lookup j "peerAddress") String.of_json
  ; bgp_options =
      Aws.Util.option_map (Aws.Json.lookup j "bgpOptions") RouteServerBgpOptions.of_json
  ; bgp_status =
      Aws.Util.option_map (Aws.Json.lookup j "bgpStatus") RouteServerBgpStatus.of_json
  ; bfd_status =
      Aws.Util.option_map (Aws.Json.lookup j "bfdStatus") RouteServerBfdStatus.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
