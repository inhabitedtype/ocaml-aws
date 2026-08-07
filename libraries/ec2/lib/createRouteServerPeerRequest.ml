open Aws.BaseTypes

type t =
  { route_server_endpoint_id : String.t
  ; peer_address : String.t
  ; bgp_options : RouteServerBgpOptionsRequest.t
  ; dry_run : Boolean.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ~route_server_endpoint_id
    ~peer_address
    ~bgp_options
    ?dry_run
    ?(tag_specifications = [])
    () =
  { route_server_endpoint_id; peer_address; bgp_options; dry_run; tag_specifications }

let parse xml =
  Some
    { route_server_endpoint_id =
        Aws.Xml.required
          "RouteServerEndpointId"
          (Aws.Util.option_bind (Aws.Xml.member "RouteServerEndpointId" xml) String.parse)
    ; peer_address =
        Aws.Xml.required
          "PeerAddress"
          (Aws.Util.option_bind (Aws.Xml.member "PeerAddress" xml) String.parse)
    ; bgp_options =
        Aws.Xml.required
          "BgpOptions"
          (Aws.Util.option_bind
             (Aws.Xml.member "BgpOptions" xml)
             RouteServerBgpOptionsRequest.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("BgpOptions", RouteServerBgpOptionsRequest.to_query v.bgp_options))
       ; Some (Aws.Query.Pair ("PeerAddress", String.to_query v.peer_address))
       ; Some
           (Aws.Query.Pair
              ("RouteServerEndpointId", String.to_query v.route_server_endpoint_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("BgpOptions", RouteServerBgpOptionsRequest.to_json v.bgp_options)
       ; Some ("PeerAddress", String.to_json v.peer_address)
       ; Some ("RouteServerEndpointId", String.to_json v.route_server_endpoint_id)
       ])

let of_json j =
  { route_server_endpoint_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RouteServerEndpointId"))
  ; peer_address =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PeerAddress"))
  ; bgp_options =
      RouteServerBgpOptionsRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "BgpOptions"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
